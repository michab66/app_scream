/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream.frontend;

import java.util.Objects;

import de.michab.scream.fcos.Number;
import de.michab.scream.util.SourcePosition;

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
        End,
        Eof,
        Label,
        LabelReference,
        List,
        Number,
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
     * The token's source code position.
     */
    private final SourcePosition _sourcePosition;

    private Token( Tk type, Object value, SourcePosition sourcePosition )
    {
        _type = type;
        _value = value;

        _sourcePosition = Objects.requireNonNull( sourcePosition );
    }

    /**
     * Creates a new Token for the given type.
     */
    public Token( Tk type, SourcePosition sourcePosition )
    {
        this( type, Void.class, sourcePosition );
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
    public Token( Tk type, String value, SourcePosition sourcePosition )
    {
        this( type, (Object)value, sourcePosition );

        if ( type != Tk.String &&
                type != Tk.Symbol )
            throw new IllegalArgumentException( "Type neither string nor symbol." );
    }

    /**
     * Create a token for the given value and set the type accordingly.
     */
    public Token( Number value, SourcePosition sourcePosition )
    {
        this( Tk.Number, value, sourcePosition );
    }

    /**
     * Create a token for the given value and set the type accordingly.
     */
    public Token( char value, SourcePosition sourcePosition )
    {
        this( Tk.Char, Character.valueOf( value ), sourcePosition );
    }

    /**
     * Create a token for the given value and set the type accordingly.
     */
    public Token( boolean value, SourcePosition sourcePosition )
    {
        this( Tk.Boolean, Boolean.valueOf( value ), sourcePosition);
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
        case Number:
            result = "TkNumber( " + _value + " )";
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
                "%s%n%s",
                result,
                _sourcePosition );
    }

    /**
     * Access the token's type.
     *
     * @return This <code>Token</code>'s type.
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
    public Number numberValue()
    {
        try
        {
            return (Number)_value;
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
