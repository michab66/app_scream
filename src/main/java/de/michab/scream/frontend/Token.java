/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.frontend;

import de.michab.scream.fcos.SchemeDouble;
import de.michab.scream.fcos.SchemeInteger;

/**
 * Represents a Token in Scream's front end.  This is independent of the
 * concrete scanner generator used.
 *
 * @see de.michab.scream.frontend.SchemeParser
 */
public final class Token
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
     * The type of the token.
     */
    private final Tk _type;

    /**
     * The token's value.  This corresponds to the type of the token.  For
     * tokens without a value this is set to the Void.class,
     * never <code>null</code>.
     */
    private final Object _value;

    /**
     * The file containing the token definition.
     */
    private final String _filename;

    /**
     * The line where the token is defined.
     */
    private final int _line;

    /**
     * The column where the token is defined.
     */
    private final int _column;

    private Token( Tk type, Object value, int line, int column, String filename )
    {
        _type = type;
        _value = value;
        _line = line;
        _column = column;
        _filename = filename;
    }

    /**
     * Creates a new Token for the given type.
     */
    public Token( Tk type, int line, int column, String filename )
    {
        this( type, Void.class, line, column, filename );
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
    public Token( Tk type, String value, int line, int column, String filename )
    {
        this( type, (Object)value, line, column, filename );

        if ( type != Tk.String &&
                type != Tk.Symbol )
            throw new IllegalArgumentException( "Type neither string nor symbol." );
    }

    /**
     * Create a token for the given value and set the type accordingly.
     */
    public Token( SchemeInteger value, int line, int column, String filename )
    {
        this( Tk.Integer, value, line, column, filename );
    }

    /**
     * Create a token for the given value and set the type accordingly.
     */
    public Token( char value, int line, int column, String filename )
    {
        this( Tk.Char, Character.valueOf( value ), line, column, filename );
    }

    /**
     * Create a token for the given value and set the type accordingly.
     */
    public Token( boolean value, int line, int column, String filename )
    {
        this( Tk.Boolean, Boolean.valueOf( value ), line, column, filename );
    }

    /**
     * Create a token for the given value and set the type accordingly.
     */
    public Token( SchemeDouble value, int line, int column, String filename )
    {
        this( Tk.Double, value, line, column, filename );
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
            result = "TkString( \"" + _value + "\" )";
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

        return String.format(
                "%s%n%s( %d, %d )",
                result,
                _filename,
                _line,
                _column );
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
