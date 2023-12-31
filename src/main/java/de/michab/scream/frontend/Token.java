/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.frontend;

import org.smack.util.StringUtil;

import de.michab.scream.fcos.SchemeDouble;
import de.michab.scream.fcos.SchemeInteger;

/**
 * Represents a Token in Scream's front end.  This is independent of the
 * concrete scanner generator used.
 *
 * @see de.michab.scream.frontend.SchemeParser
 */
public class Token
{
    /**
     * The Scheme tokens used by the frontend.
     */
    public enum Tk {
        Array,
        Boolean,
        Bytevector,
        Char,
        DatumComment,
        Dot,
        Double,
        End,
        Eof,
        Integer,
        Label,
        LabelReference,
        List,
        String,
        Symbol,
        QuasiQuote,
        Quote,
        Unquote,
        UnquoteSplicing
    }

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

    private final int _line;
    private final int _column;

    /**
     * Creates a new Token for the given type.
     */
    public Token( Tk type, int line, int column )
    {
        _type = type;
        _value = Void.class;
        _line = line;
        _column = column;
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
    public Token( Tk type, String value, int line, int column )
    {
        if ( type != Tk.String &&
                type != Tk.Symbol )
            throw new IllegalArgumentException( "Type neither string nor symbol." );

        _type = type;
        _value = value;
        _line = line;
        _column = column;
    }

    /**
     * Create a token for the given value and set the type accordingly.
     */
    public Token( SchemeInteger value, int line, int column )
    {
        _type = Tk.Integer;
        _value = value;
        _line = line;
        _column = column;
    }

    /**
     * Create a token for the given value and set the type accordingly.
     */
    public Token( char value, int line, int column )
    {
        _type = Tk.Char;
        _value = Character.valueOf( value );
        _line = line;
        _column = column;
    }

    /**
     * Create a token for the given value and set the type accordingly.
     */
    public Token( boolean value, int line, int column )
    {
        _type = Tk.Boolean;
        _value = Boolean.valueOf( value );
        _line = line;
        _column = column;
    }

    /**
     * Create a token for the given value and set the type accordingly.
     */
    public Token( SchemeDouble value, int line, int column )
    {
        _type = Tk.Double;
        _value = value;
        _line = line;
        _column = column;
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

        return result + StringUtil.EOL + _line + ", " + _column;
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
    public SchemeDouble doubleValue()
    {
        try
        {
            return (SchemeDouble)_value;
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
    public SchemeInteger integerValue()
    {
        try
        {
            return (SchemeInteger)_value;
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
