/* $Id: SchemeCharacter.java 789 2015-01-10 23:13:22Z Michael $
 *
 * Scream / Kernel
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2002 Michael G. Binz
 */
package de.michab.scream;

import de.michab.scream.util.WeakValueMap;

/**
 * Represents the Scheme character type.
 */
public class SchemeCharacter
extends FirstClassObject
{
    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#getTypename()
     */
    public static final String TYPE_NAME = "char";

    /**
     * The character cache.
     */
    private static WeakValueMap<Character, SchemeCharacter> _flyweigths =
            new WeakValueMap<Character, SchemeCharacter>();

    /**
     * This is the actual character we encapsulate.  Note that a character in Java
     * has 16 bits.
     */
    private final char _character;

    /**
     * The newline character.
     */
    public static final SchemeCharacter NEWLINE = saveCreateObject( '\n' );

    /**
     * The tab character.
     */
    public static final SchemeCharacter TAB = saveCreateObject( '\t' );

    /**
     * The space character.
     */
    public final static SchemeCharacter SPACE = saveCreateObject( ' ' );

    /**
     * Constructor.
     *
     * @param value The character value for the new instance.
     */
    private SchemeCharacter( char value )
    {
        _character = value;
        setConstant( true );
    }

    /**
     * The public character factory.  The type is integer to be callable from
     * Scream itself since there is no automatic conversion from integer to
     * character.
     *
     * @param value The integer value for the requested character.
     * @return A new <code>SchemeCharacter</code> instance.
     */
    static public SchemeCharacter createObject( int value )
    {
        return saveCreateObject( (char)value );
    }

    /**
     * The private character factory.  The difference to the public
     * <code>createObject()</code> method is the type of the operation's
     * argument.
     *
     * @param value The character value for the new instance.
     * @return A new <code>SchemeCharacter</code> instance.
     */
    static private SchemeCharacter saveCreateObject( char value )
    {
        Character k = Character.valueOf( value );

        SchemeCharacter result = _flyweigths.get( k );

        if ( result == null )
        {
            result = new SchemeCharacter( value );
            _flyweigths.put( k, result );
        }

        return result;
    }

    /**
     * Access this object's value as a character.
     *
     * @return The primitive character value.
     */
    public char asCharacter()
    {
        return _character;
    }

    /**
     * Access this object's value as an integer.
     *
     * @return The character's value as a primitive integer.
     */
    public int asInteger()
    {
        return asCharacter();
    }

    /**
     * Create a string from this object suitable for the read() operation.
     *
     * @return A string representation for the character object.
     * @see java.lang.Object#toString
     */
    @Override
    public String toString()
    {
        if ( '\n' == _character )
            return "#\\newline";
        else if ( ' ' == _character )
            return "#\\space";
        else
            return "#\\" + _character;
    }

    /**
     * Convert the <code>SchemeCharacter</code> into its native representation in
     * the Java type system.  This will result in a mapping to
     * <code>java.lang.Character</code>.
     *
     * @return References to <code>java.lang.Character</code> instances.
     */
    @Override
    public Object convertToJava()
    {
        return Character.valueOf( _character );
    }
}
