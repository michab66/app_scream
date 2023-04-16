/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.fcos;

import java.util.Arrays;
import java.util.Objects;

import de.michab.scream.RuntimeX;

/**
 * Represents the bytevector type.
 */
public final class Bytevector
    extends FirstClassObject
{
    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = "bytevector";

    /**
     * The bytes.
     */
    private final byte[] _vector;

    /**
     * Create an instance.
     */
    public Bytevector( byte ... content )
    {
        Objects.requireNonNull( content );

        _vector = Arrays.copyOf( content, content.length );
    }
    public Bytevector( int length )
    {
        _vector = new byte[ length ];
        setConstant( true );
    }

    public Bytevector set( int idx, int value ) throws RuntimeX
    {
        try
        {
            _vector[idx] = (byte)value;

            return this;
        }
        catch ( ArrayIndexOutOfBoundsException e )
        {
            throw RuntimeX.mIndexOutOfBounds( idx );
        }
    }

    public int get( int idx ) throws RuntimeX
    {
        try
        {
            return 0 | _vector[idx];
        }
        catch ( ArrayIndexOutOfBoundsException e )
        {
            throw RuntimeX.mIndexOutOfBounds( idx );
        }
    }

    public int size()
    {
        return _vector.length;
    }

    /**
     * Tests equivalence to another object.
     *
     * @param other The other object used for the comparison.
     * @return {@code true} if the objects were equal.
     * @see FirstClassObject#eq
     */
    @Override
    public boolean eq( FirstClassObject other )
    {
        return equals( other );
    }

    /**
     * Transform this to its string representation.
     *
     * @return A string representation for this object.
     * @see FirstClassObject#toString
     */
    @Override
    public String toString()
    {
        if ( _vector.length == 0 )
            return "#u8()";

        StringBuilder result = new StringBuilder( "#u8(" );

        for ( var c : _vector )
        {
            result.append( 0 | c );
            result.append( " " );
        }

        result.setCharAt( result.length()-1, ')' );

        return result.toString();
    }

    /**
     * @return The byte array.
     */
    @Override
    public Object toJava()
    {
        return Arrays.copyOf( _vector, _vector.length );
    }

    /**
     * The implementation of the standard {@code java.lang.Object.hashCode()}
     * method.
     *
     * @return An hashcode for the object instance.
     */
    @Override
    public int hashCode()
    {
        return _vector.hashCode();
    }

    /**
     *
     */
    @Override
    public boolean equals( Object other )
    {
        if ( other == Cons.NIL )
            return false;

        try
        {
            return 0 == Arrays.compare(
                    _vector,
                    ((Bytevector)other)._vector );
        }
        catch ( ClassCastException e )
        {
            return false;
        }
    }
}
