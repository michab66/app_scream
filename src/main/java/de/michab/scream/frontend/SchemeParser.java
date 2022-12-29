/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.frontend;

import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;

import de.michab.scream.Cons;
import de.michab.scream.FirstClassObject;
import de.michab.scream.Port;
import de.michab.scream.SchemeBoolean;
import de.michab.scream.SchemeCharacter;
import de.michab.scream.SchemeDouble;
import de.michab.scream.SchemeInteger;
import de.michab.scream.SchemeString;
import de.michab.scream.ScreamException.Code;
import de.michab.scream.Symbol;
import de.michab.scream.Vector;
import de.michab.scream.frontend.Token.Tk;

/**
 * An LL(1) parser for scheme.  Parses the non-terminal <datum> described in
 * r5rs.  No special provisions are met for parsing keywords and related
 * expressions, this has to be handled by the interpreter.
 *
 * @author Michael G. Binz
 */
public class SchemeParser
{
    /**
     * A flyweight quote symbol.
     */
    private final static FirstClassObject QUOTE_SYMBOL
        = Symbol.createObject( "quote" );


    /**
     * A flyweight quasiquote symbol.
     */
    private final static FirstClassObject QUASIQUOTE_SYMBOL
        = Symbol.createObject( "quasiquote" );

    /**
     * A flyweight unquote symbol.
     */
    public final static FirstClassObject UNQUOTE_SYMBOL
        = Symbol.createObject( "unquote" );

    /**
     * A flyweight unquote-splicing symbol.
     */
    public final static FirstClassObject UNQUOTE_SPLICING_SYMBOL
        = Symbol.createObject( "unquote-splicing" );

    /**
     * This parser's scanner.
     */
    private final SchemeScanner _scanner;

    /**
     * Temporary storage for a peeked token.  Base for a one token lookahead.
     * Note: The scanner is a LL(1) one.  Used only by the methods
     * getNextToken() and peekNextToken().
     *
     * @label lookahead
     */
    private Token _peeked = null;

    /**
     * Construct a parser for the passed reader.  Creates its scanner and operates
     * the scanner in a synchronous way.  Currently it is possible to create a
     * synchronous parser by specifying a null sink.
     *
     * @param source A reader that delivers the parser input.
     */
    public SchemeParser( Reader source )
    {
        _scanner = new SchemeScanner( source );
    }

    /**
     * Construct a parser for the passed string.
     *
     * @param source The string to parse.
     */
    public SchemeParser( String source )
    {
        this( new StringReader( source ) );
    }

    /**
     * Get the next token from whatever scanner we have.
     *
     * @return The next token read, null on EOF.
     * @throws FrontendX In case of an error.
     * @see de.michab.scream.scanner.SchemeParserTest#peekNextToken
     */
    private Token getNextToken()
            throws FrontendX
    {
        Token result;

        if ( null == _peeked )
            result = _scanner.getNextToken();
        else
        {
            result = _peeked;
            _peeked = null;
        }

        return result;
    }

    /**
     * Get the lookahead token.  The next call to getNextToken() will just
     * return the same token.  Repeated calls of this method also result always
     * in the same Token object.
     *
     * @return The next token, null on EOF.
     * @throws FrontendX In case of an error.
     * @see de.michab.scream.scanner.SchemeParserTest#getNextToken
     */
    private Token peekNextToken()
            throws FrontendX
    {
        // If we do not have a lookahead token...
        if ( null == _peeked )
            // ...we get one.
            _peeked = _scanner.getNextToken();

        // Return the lookahead.
        return _peeked;
    }

    /**
     * Parses a Scheme array.
     *
     * @return An array structure.
     * @throws FrontendX In case of an error.
     */
    private FirstClassObject parseArray()
            throws FrontendX
    {
        ArrayList<FirstClassObject> collector =
                new ArrayList<>();

        // As long as we find no array end...
        while ( Tk.End != peekNextToken().getType() )
            // ...just add the expressions to our local vector.
            collector.add( parseDatum() );

        // Consume the End token.
        getNextToken();

        FirstClassObject[] array = new FirstClassObject[ collector.size() ];

        return new Vector( collector.toArray( array ), false );
    }

    /**
     * Parses a Scheme list.
     *
     * @return A list structure.
     * @throws FrontendX In case of an error.
     */
    private FirstClassObject parseList()
            throws FrontendX
    {
        // If this is the end of the list...
        if ( Tk.End == peekNextToken().getType() )
        {
            // ...consume the token...
            getNextToken();
            // ...and return the empty list.
            return Cons.NIL;
        }

        // Read the car part of the list.
        FirstClassObject car = parseDatum();
        // ...and prepare for the cdr.
        FirstClassObject cdr = Cons.NIL;

        // Check if we are reading a proper list.
        if ( Tk.Dot == peekNextToken().getType() )
        {
            // Not proper.  Consume token...
            getNextToken();
            // ...and fill cdr.
            cdr = parseDatum();

            // List has to be finished.
            if ( Tk.End != getNextToken().getType() )
                throw new FrontendX(
                        Code.PARSE_EXPECTED,
                        ")" );
        }
        else
            cdr = parseList();

        // Finally create the cons cell and return that.
        return new Cons( car, cdr );
    }

    /**
     * Parses any scheme expression.
     *
     * @return An expression.
     * @throws FrontendX In case of an error.
     */
    private FirstClassObject parseDatum()
            throws FrontendX
    {
        Token token = getNextToken();

        switch ( token.getType() )
        {
        case Char:
            return SchemeCharacter.createObject( token.characterValue() );

        case Symbol:
            return Symbol.createObject( token.stringValue() );

        case Integer:
            return SchemeInteger.createObject( token.integerValue() );

        case Double:
            return SchemeDouble.createObject( token.doubleValue() );

        case Array:
            return parseArray();

        case List:
            return parseList();

        case String:
            return new SchemeString( token.stringValue() );

        case Quote:
            return new Cons( QUOTE_SYMBOL,
                    new Cons( parseDatum(), Cons.NIL ) );

        case QuasiQuote:
            return new Cons( QUASIQUOTE_SYMBOL,
                    new Cons( parseDatum(), Cons.NIL ) );

        case Unquote:
            return new Cons( UNQUOTE_SYMBOL,
                    new Cons( parseDatum(), Cons.NIL ) );

        case UnquoteSplicing:
            return new Cons( UNQUOTE_SPLICING_SYMBOL,
                    new Cons( parseDatum(), Cons.NIL ) );

        case Boolean:
            return
                    SchemeBoolean.createObject( token.booleanValue() );

        case Eof:
            throw new FrontendX( Code.PARSE_UNEXPECTED_EOF );

        default:
            throw new FrontendX(
                    Code.PARSE_UNEXPECTED,
                    token.toString() );
        }
    }

    /**
     * Parses any Scheme expression.  Differs from the parseDatum() method
     * above in the EOF handling.  This method returns an EOF object when
     * reaching EOF.
     *
     * @return The next Scheme expression on the token stream.
     * @throws FrontendX In case of an error.
     */
    public FirstClassObject getExpression()
            throws FrontendX
    {
        Token token = peekNextToken();

        if ( token.getType() == Tk.Eof )
            return Port.EOF;

        return parseDatum();
    }
}
