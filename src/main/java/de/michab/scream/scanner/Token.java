/* $Id: Token.java 1 2008-09-19 16:30:02Z binzm $
 *
 * Scream: Frontend
 *
 * Released under Gnu Public License
 * Copyright (c) 1998,2001 Michael G. Binz
 */
package de.michab.scream.scanner;

/**
 * Represents a Token in Scream's front end.  This is independent of the
 * concrete scanner generator used.
 *
 * @see de.michab.scream.scanner.SchemeParser
 */
public class Token
{
  /**
   * Used to implement a flyweight scheme for value-less Tokens.
   * @see createToken
   */
  static private Token[] _flyweights = new Token[ SchemeParser.MaxTokenIndex ];

  /**
   * The type of the token.  The possible types are defined as constants in the
   * SchemeParser.
   * @see de.michab.scream.scanner.SchemeParser
   */
  private final int _type;

  /**
   * The token's value.  This corresponds to the type of the token.  For
   * tokens without a value this is set to the Void.class, so this will
   * never be <code>null</code>.
   */
  private final Object _value;

  /**
   * Creates a new Token for the given type.
   */
  public Token( int type )
  {
    _type = type;
    _value = Void.class;
  }

  /**
   * Creates a new Token for the given type value pair.  Two token types are
   * string-valued, TkString and TkSymbol.  If the passed type is not one of
   * these an IllegalArgumentException is thrown.
   *
   * @param type Either SchemeParser.TkString or SchemeParser.TkSymbol.
   * @param value The token's value.
   * @throws IllegalArgumentException Wrong value for the type parameter.
   */
  public Token( int type, String value )
  {
    if ( type != SchemeParser.TkString &&
         type != SchemeParser.TkSymbol )
      throw new IllegalArgumentException( "Type neither string nor symbol." );

    _type = type;
    _value = value;
  }



  /**
   * Create a token for the given value and set the type accordingly.
   */
  public Token( long value )
  {
    _type = SchemeParser.TkInteger;
    _value = Long.valueOf( value );
  }

  /**
   * Create a token for the given value and set the type accordingly.
   */
  public Token( char value )
  {
    _type = SchemeParser.TkChar;
    _value = Character.valueOf( value );
  }

  /**
   * Create a token for the given value and set the type accordingly.
   */
  public Token( boolean value )
  {
    _type = SchemeParser.TkBoolean;
    _value = Boolean.valueOf( value );
  }

  /**
   * Create a token for the given value and set the type accordingly.
   */
  public Token( double value )
  {
    _type = SchemeParser.TkDouble;
    _value = Double.valueOf( value );
  }

  /**
   * Create a token that holds only type information.  This can be used for
   * tokens representing keywords.  In that case a token has to carry no
   * additional information besides its token type.</br>
   * This factory method ensures that only a single instance per token type
   * will be created.
   */
  public synchronized static Token createToken( int type )
  {
    // Compute the flyweight table idx.
    int flyweightIdx = type-1;
    // Access the table.
    Token result = _flyweights[ flyweightIdx ];
    // Check if we received a token...
    if ( result == null )
    {
      // ...and create one if not...
      result = new Token( type );
      // ...and put that into the flyweight table.
      _flyweights[ flyweightIdx ] = result;
    }

    return result;
  }

  /**
   * @see java.lang.Object#toString
   */
  @Override
public String toString()
  {
    String result;

    switch ( _type )
    {
      case SchemeParser.TkSymbol:
        result = "TkSymbol( " + _value + " )";
        break;
      case SchemeParser.TkInteger:
        result = "TkInteger( " + _value + " )";
        break;
      case SchemeParser.TkDouble:
        result = "TkDouble( " + _value + " )";
        break;
      case SchemeParser.TkArray:
        result = "TkArray";
        break;
      case SchemeParser.TkList:
        result = "TkList";
        break;
      case SchemeParser.TkEnd:
        result = "TkEnd";
        break;
      case SchemeParser.TkString:
        result = "TkString( " + _value + " )";
        break;
      case SchemeParser.TkQuote:
        result = "TkQuote";
        break;
      case SchemeParser.TkDot:
        result = "TkDot";
        break;
      case SchemeParser.TkBoolean:
        result = "TkBoolean(" + _value + " )";
        break;
      case SchemeParser.TkChar:
        result = "TkChar(" + _value + " )";
        break;
      case SchemeParser.TkEof:
        result = "TkEof";
        break;
      default:
        result = "UnknownTokenValue";
        break;
    }

    return result;
  }

  /**
   * Access the token's type as defined as integer constants in SchemeParser.
   *
   * @return This <code>Token</code>'s type.
   * @see SchemeParser
   */
  public int getType()
  {
    return _type;
  }

  /**
   * Get the token's string value.  This is only successful if the token is
   * also of type SchemeParser.TkString.  If that is not the case, the program
   * will be stopped.
   */
  public String stringValue()
  {
    try
    {
      return (String)_value;
    }
    catch ( ClassCastException e )
    {
      throw new InternalError( "Not a string." );
    }
  }

  /**
   * Get the token's double value.  This is only successful if the token is
   * of type SchemeParser.TkDouble.  If that is not the case, the program
   * will be stopped.
   */
  public double doubleValue()
  {
    try
    {
      return ((Double)_value).doubleValue();
    }
    catch ( ClassCastException e )
    {
      throw new InternalError( "Not a double." );
    }
  }

  /**
   * Get the token's character value.  This is only successful if the token is
   * of type SchemeParser.TkChar.  If that is not the case, the program
   * will be stopped.
   */
  public char characterValue()
  {
    try
    {
      return ((Character)_value).charValue();
    }
    catch ( ClassCastException e )
    {
      throw new InternalError( "Not a character." );
    }
  }

  /**
   * Get the token's integer value.  This is only successful if the token is
   * of type SchemeParser.TkInteger.  If that is not the case, the program
   * will be stopped.
   */
  public long integerValue()
  {
    try
    {
      return ((Long)_value).longValue();
    }
    catch ( ClassCastException e )
    {
      throw new InternalError( "Not an integer." );
    }
  }

  /**
   * Get the token's boolean value.  This is only successful if the token is
   * of type SchemeParser.TkBoolean.  If that is not the case, the program
   * will be stopped.
   */
  public boolean booleanValue()
  {
    try
    {
      return ((Boolean)_value).booleanValue();
    }
    catch ( ClassCastException e )
    {
      throw new InternalError( "Not a boolean." );
    }
  }
}
