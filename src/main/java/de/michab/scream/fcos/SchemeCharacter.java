/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import org.smack.util.collections.WeakMapWithProducer;

/**
 * Represents the Scheme character type.
 */
public class SchemeCharacter
    extends FirstClassObject
{
    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = "char";

    /**
     * The character cache.
     */
    private static WeakMapWithProducer<Character, SchemeCharacter> _flyweigths =
            new WeakMapWithProducer<Character, SchemeCharacter>( c -> new SchemeCharacter( c ) );

    /**
     * This is the actual character we encapsulate.  Note that a character in Java
     * has 16 bits.
     */
    private final char _character;

    /**
     * The newline character.
     */
    public static final SchemeCharacter NEWLINE = _flyweigths.get( '\n' );

    /**
     * The tab character.
     */
    public static final SchemeCharacter TAB = _flyweigths.get( '\t' );

    /**
     * The space character.
     */
    public final static SchemeCharacter SPACE = _flyweigths.get( ' ' );

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
     * @return A new {@code SchemeCharacter} instance.
     */
    static public SchemeCharacter createObject( int value )
    {
        return createObject( (char)value );
    }

    /**
     * The public character factory.  The type is integer to be callable from
     * Scream itself since there is no automatic conversion from integer to
     * character.
     *
     * @param value The integer value for the requested character.
     * @return A new {@code SchemeCharacter} instance.
     */
    static public SchemeCharacter createObject( char value )
    {
        return _flyweigths.get( value );
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
     * Convert the {@code SchemeCharacter} into its native representation in
     * the Java type system.  This will result in a mapping to
     * {@code java.lang.Character}.
     *
     * @return References to {@code java.lang.Character} instances.
     */
    @Override
    public Object toJava()
    {
        return Character.valueOf( _character );
    }
}
