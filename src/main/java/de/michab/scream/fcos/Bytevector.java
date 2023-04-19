/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2023 Michael G. Binz
 */
package de.michab.scream.fcos;

import java.util.Arrays;
import java.util.Objects;

import de.michab.scream.RuntimeX;
import de.michab.scream.util.Scut;

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
     * Create an instance. Used from the Scream runtime.
     */
    public Bytevector( byte ... content )
    {
        Objects.requireNonNull( content );

        _vector = Arrays.copyOf( content, content.length );
    }
    public Bytevector( Cons bytes ) throws RuntimeX
    {
        Objects.requireNonNull( bytes );

        if ( ! Cons.isProper( bytes ) )
            throw RuntimeX.mExpectedProperList( bytes );

        if ( 0 == Cons.length( bytes ) )
        {
            _vector = new byte[0];
            return;
        }

        _vector = new byte[assertLength( Cons.length( bytes ) )];

        int count = 0;
        for ( var c : bytes )
        {
            SchemeInteger i = Scut.as( SchemeInteger.class, c );

            _vector[count++] = assertByte( i.asLong() );
        }
    }
    public Bytevector( long length ) throws RuntimeX
    {
        this( length, 0 );
    }
    public Bytevector( long length, long filler ) throws RuntimeX
    {
        _vector = new byte[ assertIndex( length ) ];

        byte bFiller = assertByte( filler );

        Arrays.fill( _vector, bFiller );
    }

    /**
     * Set a byte in the bytevector.
     *
     * @param idx The index to use.
     * @param value The value to set.
     * @return The bytevector.
     * @throws RuntimeX If the bytevector is constant or the index
     * is out of bounds.
     */
    public Bytevector set( long idx, long value ) throws RuntimeX
    {
        if ( isConstant() )
            throw RuntimeX.mCannotModifyConstant( this );

        try
        {
            _vector[assertIndex( idx )] = assertByte( value );

            return this;
        }
        catch ( ArrayIndexOutOfBoundsException e )
        {
            throw RuntimeX.mIndexOutOfBounds( idx );
        }
    }

    /**
     * Read a byte in the bytevector.
     *
     * @param idx The index to read.
     * @return The value at the index.
     * @throws RuntimeX If the bytevector index is out of bounds.
     */
    public int get( long idx ) throws RuntimeX
    {
        try
        {
            return 0xff & _vector[assertIndex( idx )];
        }
        catch ( ArrayIndexOutOfBoundsException e )
        {
            throw RuntimeX.mIndexOutOfBounds( idx );
        }
    }

    /**
     * @return The size of the bytevector.
     */
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
    public boolean equal( FirstClassObject other )
    {
        return equals( other );
    }

    /**
     * Transform this to its string representation.
     *
     * @return A string representation of this object.
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
            String toAppend = String.format(
                    "#x%02x ",
                    Byte.toUnsignedInt( c ) );

            result.append( toAppend );
        }

        // Replace the trailing space.
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

    @Override
    public Bytevector copy()
    {
        return new Bytevector( _vector );
    }
    public Bytevector copy( long start ) throws RuntimeX
    {
        return copy( start, _vector.length );
    }
    /**
     *
     * @param start
     * @param to exclusive
     * @return
     * @throws RuntimeX
     */
    public Bytevector copy( long start, long to ) throws RuntimeX
    {
        var iStart =
                assertIndex( start );
        var iTo =
                assertIndex( to );

        if ( start < 0 || start > _vector.length )
            throw RuntimeX.mIndexOutOfBounds( start );
        if ( to < 0 || to > _vector.length )
            throw RuntimeX.mIndexOutOfBounds( to );
        if ( start > to )
            throw RuntimeX.mIllegalArgument( "start > to" );

        return new Bytevector(
                Arrays.copyOfRange( _vector, iStart, iTo ) );
    }

    public static int assertLength( long idx ) throws RuntimeX
    {
        if ( idx < 0 || idx > Integer.MAX_VALUE )
            throw RuntimeX.mRangeExceeded(
                    SchemeInteger.createObject( idx ),
                    "[0.." + Integer.MAX_VALUE + "]" );

        return (int)idx;
    }

    public static int assertIndex( long idx ) throws RuntimeX
    {
        if ( idx < 0 || idx > Integer.MAX_VALUE )
            throw RuntimeX.mRangeExceeded(
                    SchemeInteger.createObject( idx ),
                    "[0.." + Integer.MAX_VALUE + "]" );

        return (int)idx;
    }

    public static byte assertByte( long idx ) throws RuntimeX
    {
        if ( idx < 0 || idx > 0xff )
            throw RuntimeX.mRangeExceeded(
                    SchemeInteger.createObject( idx ),
                    "[0..255]" );

        return (byte)(0xff & idx);
    }
}
