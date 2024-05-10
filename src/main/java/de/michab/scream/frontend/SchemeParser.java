/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream.frontend;

import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.smack.util.Holder;

import de.michab.scream.Raise;
import de.michab.scream.RuntimeX;
import de.michab.scream.fcos.Bool;
import de.michab.scream.fcos.Bytevector;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Number;
import de.michab.scream.fcos.Port;
import de.michab.scream.fcos.SchemeCharacter;
import de.michab.scream.fcos.SchemeString;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.fcos.Vector;
import de.michab.scream.frontend.Token.Tk;
import de.michab.scream.util.Scut;
import de.michab.scream.util.Scut.ConsumerX;

/**
 * An LL(1) parser for scheme.  Parses the non-terminal {@code <datum>}
 * described in r7rs.  No special provisions are met for parsing keywords
 * and related expressions, this has to be handled by the interpreter.
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
     * Construct a parser for the passed reader.
     *
     * @param source A reader that delivers the parser input.
     */
    public SchemeParser( Reader source, String filename )
    {
        _scanner = new SchemeScanner( source, filename, this );
    }

    /**
     * Construct a parser for the passed string.
     *
     * @param source The string to parse.
     */
    public SchemeParser( String source, String filename )
    {
        this( new StringReader( source ), filename );
    }
    public SchemeParser( String source )
    {
        this( new StringReader( source ), "SchemeParser<init>(String)" );
    }

    /**
     * Get the next token.
     *
     * @return The next token read, null on EOF.
     * @throws RuntimeX In case of an error.
     */
    private Token getNextToken()
            throws RuntimeX
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
     * @throws RuntimeX In case of an error.
     */
    private Token peekNextToken()
            throws RuntimeX
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
     * @param labels
     *
     * @return An array structure.
     * @throws RuntimeX In case of an error.
     */
    private FirstClassObject parseArray(Map<Long, Holder<FirstClassObject>> labels)
            throws RuntimeX
    {
        ArrayList<FirstClassObject> collector =
                new ArrayList<>();

        // As long as we find no array end...
        while ( Tk.End != peekNextToken().getType() )
            // ...just add the expressions to our local vector.
            collector.add( parseDatum(labels) );

        // Consume the End token.
        getNextToken();

        FirstClassObject[] array = new FirstClassObject[ collector.size() ];

        return new Vector( collector.toArray( array ), false );
    }

    /**
     * Parses a Scheme bytevector.
     *
     * @return An array structure.
     * @throws RuntimeX In case of an error.
     */
    private FirstClassObject parseBytevector()
            throws RuntimeX
    {
        ArrayList<Byte> collector =
                new ArrayList<>();

        while ( Tk.End != peekNextToken().getType() )
        {
            Token token = getNextToken();

            if ( Tk.Number != token.getType() )
                throw Raise.mParseExpected( Tk.Number );

            collector.add(
                    Scut.assertByte( token.numberValue().asLong() ) );
        }

        // Consume the End token.
        getNextToken();

        byte[] result = new byte[ collector.size() ];
        for ( int i = 0 ; i < result.length ; i++ )
            result[i] = collector.get( i );

        return new Bytevector( result );
    }

    /**
     * Parses a Scheme list.
     * @param labels
     *
     * @return A list structure.
     * @throws RuntimeX In case of an error.
     */
    private FirstClassObject parseList(Map<Long, Holder<FirstClassObject>> labels)
    //HashMap<Long,Cons> labelMap )
            throws RuntimeX
    {
        Cons result = Cons.NIL;
        Cons resultTail = Cons.NIL;

        while ( true )
        {

            if ( Tk.End == peekNextToken().getType() )
            {
                getNextToken();
                return result;
            }

            Cons c = new Cons( Cons.NIL, Cons.NIL );

            if ( result == Cons.NIL ) {
                result = c;
                resultTail = c;
            }
            else
            {
                resultTail.setCdr( c );
                resultTail = c;
            }

            c.setCar( parseDatum(labels) );

            // Check if we are reading a proper list.
            if ( Tk.Dot == peekNextToken().getType() )
            {
                // Not proper.
                getNextToken();
                c.setCdr( parseDatum(labels) );

                // List has to be finished.
                if ( Tk.End != getNextToken().getType() )
                    throw Raise.mParseExpected( Tk.End );

                return result;
            }
        }
    }

    private FirstClassObject parseLabeledDatum(
            Number number,
            Map<Long, Holder<FirstClassObject>> labels )
        throws RuntimeX
    {
        Long labelNumber = number.asLong();

        // TODO adjust type.
        if ( labels.containsKey( labelNumber ) )
            throw Raise.mDuplicateElement( number );

        Holder<FirstClassObject> fcoh =
                new Holder<FirstClassObject>( Cons.NIL ) {
            @Override
            public String toString()
            {
                return "";
            }
        };

        labels.put( labelNumber, fcoh );

        var result = parseDatum( labels );

        fcoh.set( result );

        return result;
    }

    private FirstClassObject parseLabelReference(
            Number number,
            Map<Long, Holder<FirstClassObject>> labels )
        throws RuntimeX
    {
        Long labelNumber = number.asLong();

        var holder = labels.get( labelNumber );

        // TODO adjust type.
        if ( holder == null )
            throw Raise.mFieldNotFound( "" + number );

        return new Proxy( labelNumber, holder );
    }

    /**
     * Parses any scheme expression.
     *
     * @return An expression.
     * @throws RuntimeX In case of an error.
     */
    private FirstClassObject parseDatum( Map<Long, Holder<FirstClassObject>> labels )
            throws RuntimeX
    {
        Token token = getNextToken();

        switch ( token.getType() )
        {
        case Bytevector:
            return parseBytevector();

        case Char:
            return SchemeCharacter.createObject( token.characterValue() );

        case Symbol:
            return Symbol.createObject( token.stringValue() );

        case Number:
            return token.numberValue();

        case Array:
            return parseArray( labels );

        case List:
            return parseList(labels);

        case String:
            return SchemeString.makeEscaped( token.stringValue() );

        case Quote:
            return new Cons( QUOTE_SYMBOL,
                    new Cons( parseDatum( labels ) ) );

        case QuasiQuote:
            return new Cons( QUASIQUOTE_SYMBOL,
                    new Cons( parseDatum( labels ) ) );

        case Unquote:
            return new Cons( UNQUOTE_SYMBOL,
                    new Cons( parseDatum( labels ) ) );

        case UnquoteSplicing:
            return new Cons( UNQUOTE_SPLICING_SYMBOL,
                    new Cons( parseDatum( labels ) ) );

        case Boolean:
            return Bool.createObject( token.booleanValue() );

        case Label:
            return parseLabeledDatum( token.numberValue(), labels  );

        case LabelReference:
            return parseLabelReference( token.numberValue(), labels );

        case Eof:
            throw Raise.mParseUnexpectedEof();

        default:
            throw Raise.mParseUnexpected( token );
        }
    }

    /**
     * Swallows a datum, used in the scanner with datum comments.
     * @return
     * @throws RuntimeX
     */
    public FirstClassObject parseDatum() throws RuntimeX
    {
        return parseDatum( new HashMap<>() );
    }

    /**
     * Parses any Scheme expression.  Differs from the parseDatum() method
     * above in the EOF handling.  This method returns an EOF object when
     * reaching EOF.
     *
     * @return The next Scheme expression on the token stream.
     * @throws RuntimeX In case of an error.
     */
    public FirstClassObject getExpression()
            throws RuntimeX
    {
        Token token = peekNextToken();

        if ( token.getType() == Tk.Eof )
            return Port.EOF;

        Map<Long, Holder<FirstClassObject>> referenceCollector =
                new HashMap<>() ;
        var result =
                parseDatum( referenceCollector );

        if ( referenceCollector.size() == 0 )
            return result;

        visitNodes( result, this::replaceProxies );

        return result;
    }

    private void replaceProxies(
            FirstClassObject fco,
            ConsumerX<FirstClassObject> parentSetter )
        throws RuntimeX
    {
        if ( ! FirstClassObject.is( Proxy.class, fco ) )
            return;

        var proxy = Scut.as( Proxy.class, fco );

        parentSetter.accept( proxy.value() );
    }

    private void visitConsElements(
            Cons cons,
            BiConsumerX<FirstClassObject, ConsumerX<FirstClassObject>> visitor )
    throws RuntimeX
    {
        if ( cons == Cons.NIL )
            return;

        while ( true )
        {
            var currentFco = cons.getCar();

            visitNodes( currentFco, visitor );

            var next = cons.getCdr();

            visitor.accept(
                    currentFco,
                    cons::setCar );


            if ( Cons.NIL == next )
                break;
            else if ( FirstClassObject.is( Cons.class, next ) )
                cons = Scut.as( Cons.class, next );
            else
            {
                visitNodes(
                        next,
                        visitor );

                visitor.accept(
                        next,
                        cons::setCdr );
                break;
            }
        }
    }

    private void visitVectorElements(
            Vector vector,
            BiConsumerX<FirstClassObject, ConsumerX<FirstClassObject>> visitor )
            throws RuntimeX
    {
        for ( long i = 0 ; i < vector.size() ; i++ )
        {
            final var ii = i;

            var current = vector.get( ii );

            visitor.accept(
                    current,
                    replacement -> { vector.set( ii, replacement ); } );

            visitNodes( current, visitor );
        }
    }

    @FunctionalInterface
    public interface BiConsumerX<T, U> {

        /**
         * Performs this operation on the given arguments.
         *
         * @param t the first input argument
         * @param u the second input argument
         */
        void accept(T t, U u) throws RuntimeX;
    }

    private FirstClassObject visitNodes(
        FirstClassObject root,
        BiConsumerX<FirstClassObject,ConsumerX<FirstClassObject>> visitor ) throws RuntimeX
    {
        if ( root == Cons.NIL )
            ;
        else if ( FirstClassObject.is( Cons.class, root ) )
            visitConsElements( Scut.as( Cons.class, root ), visitor );
        else if ( FirstClassObject.is( Vector.class, root ) )
            visitVectorElements( Scut.as( Vector.class, root ), visitor );

        return root;
    }
}
