/* $Id: Token.java 1 2008-09-19 16:30:02Z binzm $
 *
 * Scream: Frontend
 *
 * Copyright (c) 1998,2022 Michael G. Binz
 */
package de.michab.scream.frontend;

import org.smack.util.EnumArray;

/**
 * Represents a Token in Scream's front end.  This is independent of the
 * concrete scanner generator used.
 *
 * @see de.michab.scream.frontend.SchemeParser
 */
public class Token
{
  /**
   * Used to implement a flyweight scheme for value-less Tokens.
   * @see createToken
   */
  static private EnumArray<Tk, Token> _flyweights =
          new EnumArray<Tk, Token>( Tk.class, null );

  /**
   * The type of the token.  The possible types are defined as constants in the
   * SchemeParser.
   * @see de.michab.scream.frontend.SchemeParser
   */
  private final Tk _type;

  /**
   * The token's value.  This corresponds to the type of the token.  For
   * tokens without a value this is set to the Void.class, so this will
   * never be <code>null</code>.
   */
  private final Object _value;

  /**
   * Creates a new Token for the given type.
   */
  public Token( Tk type )
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
  public Token( Tk type, String value )
  {
    if ( type != Tk.String &&
         type != Tk.Symbol )
      throw new IllegalArgumentException( "Type neither string nor symbol." );

    _type = type;
    _value = value;
  }

  /**
   * Create a token for the given value and set the type accordingly.
   */
  public Token( long value )
  {
    _type = Tk.Integer;
    _value = Long.valueOf( value );
  }

  /**
   * Create a token for the given value and set the type accordingly.
   */
  public Token( char value )
  {
    _type = Tk.Char;
    _value = Character.valueOf( value );
  }

  /**
   * Create a token for the given value and set the type accordingly.
   */
  public Token( boolean value )
  {
    _type = Tk.Boolean;
    _value = Boolean.valueOf( value );
  }

  /**
   * Create a token for the given value and set the type accordingly.
   */
  public Token( double value )
  {
    _type = Tk.Double;
    _value = Double.valueOf( value );
  }

  /**
   * Create a token that holds only type information.  This can be used for
   * tokens representing keywords.  In that case a token has to carry no
   * additional information besides its token type.</br>
   * This factory method ensures that only a single instance per token type
   * will be created.
   */
  public synchronized static Token createToken( Tk type )
  {
    // Access the table.
    Token result = _flyweights.get( type );
    // Check if we received a token...
    if ( result == null )
    {
      // ...and create one if not...
      result = new Token( type );
      // ...and put that into the flyweight table.
      _flyweights.set( type, result );
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
      case Symbol:
        result = "TkSymbol( " + _value + " )";
        break;
      case Integer:
        result = "TkInteger( " + _value + " )";
        break;
      case Double:
        result = "TkDouble( " + _value + " )";
        break;
      case Array:
        result = "TkArray";
        break;
      case List:
        result = "TkList";
        break;
      case End:
        result = "TkEnd";
        break;
      case String:
        result = "TkString( " + _value + " )";
        break;
      case Quote:
        result = "TkQuote";
        break;
      case Dot:
        result = "TkDot";
        break;
      case Boolean:
        result = "TkBoolean(" + _value + " )";
        break;
      case Char:
        result = "TkChar(" + _value + " )";
        break;
      case Eof:
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
  public Tk getType()
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
