/* $Id: SchemeParser.java 172 2009-03-19 21:21:48Z Michael $
 *
 * Scream / Frontend
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2009 Michael G. Binz
 */
package de.michab.scream.frontend;

import java.io.Reader;
import java.io.StringReader;
import java.util.logging.Logger;

import de.michab.scream.Cons;
import de.michab.scream.FirstClassObject;
import de.michab.scream.Port;
import de.michab.scream.SchemeBoolean;
import de.michab.scream.SchemeCharacter;
import de.michab.scream.SchemeDouble;
import de.michab.scream.SchemeInteger;
import de.michab.scream.SchemeString;
import de.michab.scream.Symbol;
import de.michab.scream.Vector;



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
     * The logger for this class.
     */
    private final static Logger _log =
      Logger.getLogger( SchemeParser.class.getName() );



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
   * Tokens processed by this parser
   */
  public static final int TkSymbol = 1;
  public static final int TkInteger = 2;
  public static final int TkDouble = 3;
  public static final int TkArray = 4;
  public static final int TkList = 5;
  public static final int TkEnd = 6;
  public static final int TkString = 7;
  public static final int TkQuote = 8;
  public static final int TkDot = 9;
  public static final int TkBoolean = 10;
  public static final int TkChar = 11;
  public static final int TkEof = 12;
  public static final int TkQuasiQuote = 13;
  public static final int TkUnquote = 14;
  public static final int TkUnquoteSplicing = 15;



  /**
   * A constant specifying the first unused token index.
   */
  public static final int MaxTokenIndex = 16;



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
   * Parses a scheme array.
   *
   * @return An array structure.
   * @throws FrontendX In case of an error.
   */
  private FirstClassObject parseArray()
    throws FrontendX
  {
    java.util.Vector<FirstClassObject> collector =
      new java.util.Vector<FirstClassObject>();

    // As long as we find no array end...
    while ( TkEnd != peekNextToken().getType() )
      // ...just add the expressions to our local vector.
      collector.addElement( parseDatum() );

    // Array end found.  Consume the token...
    getNextToken();
    // ...create an array of the read elements...
    FirstClassObject[] array = new FirstClassObject[ collector.size() ];
    // ...copy our collected values.
    for ( int i = array.length -1 ; i >= 0 ; i-- )
      array[i] = collector.elementAt( i );

    // Finally create a scream vector and return that one.
    return new Vector( array, false );
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
    if ( TkEnd == peekNextToken().getType() )
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
    if ( TkDot == peekNextToken().getType() )
    {
      // Not proper.  Consume token...
      getNextToken();
      // ...and fill cdr.
      cdr = parseDatum();

      // List has to be finished.
      if ( TkEnd != getNextToken().getType() )
        throw new FrontendX( "PARSE_EXPECTED",
                             new Object[]{ ")" } );
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
      case TkChar:
        return SchemeCharacter.createObject( token.characterValue() );

      case TkSymbol:
        return Symbol.createObject( token.stringValue() );

      case TkInteger:
        return SchemeInteger.createObject( token.integerValue() );

      case TkDouble:
        return SchemeDouble.createObject( token.doubleValue() );

      case TkArray:
        return parseArray();

      case TkList:
        return parseList();

      case TkString:
        return new SchemeString( token.stringValue() );

      case TkQuote:
        return new Cons( QUOTE_SYMBOL,
                         new Cons( parseDatum(), Cons.NIL ) );

      case TkQuasiQuote:
        return new Cons( QUASIQUOTE_SYMBOL,
                         new Cons( parseDatum(), Cons.NIL ) );

      case TkUnquote:
        return new Cons( UNQUOTE_SYMBOL,
                         new Cons( parseDatum(), Cons.NIL ) );

      case TkUnquoteSplicing:
        return new Cons( UNQUOTE_SPLICING_SYMBOL,
                         new Cons( parseDatum(), Cons.NIL ) );

      case TkBoolean:
        return
          SchemeBoolean.createObject( token.booleanValue() );

      case TkEof:
        throw new FrontendX( "PARSE_UNEXPECTED_EOF" );
    }

    throw new FrontendX( "PARSE_UNEXPECTED",
                         new Object[]{ token.toString() } );
  }



  /**
   * Parses any scheme expression.  Differs from the parseDatum() method
   * above in the EOF handling.  This method returns an EOF object when
   * reaching EOF.
   *
   * @return The next Scheme expression on the token stream.
   * @throws FrontendX In case of an error.
   */
  private FirstClassObject parseTopLevelDatum()
    throws FrontendX
  {
    Token token = peekNextToken();

    if ( token.getType() == TkEof )
      return Port.EOF;

    return parseDatum();
  }



  /**
   * Read a single Scheme expression.
   *
   * @return An expression.  Returns Port.EOF in case the end of the input
   * reader is reached.
   * @throws FrontendX In case of an error.
   */
  public FirstClassObject getExpression()
    throws FrontendX
  {
    return parseTopLevelDatum();
  }
}
