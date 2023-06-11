/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2023 Michael G. Binz
 */
package de.michab.scream.fcos;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
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

        _vector = new byte[Scut.assertLength( Cons.length( bytes ) )];

        int count = 0;
        for ( var c : bytes )
        {
            SchemeInteger i = Scut.as( SchemeInteger.class, c );

            _vector[count++] = Scut.assertByte( i.asLong() );
        }
    }
    public Bytevector( long length ) throws RuntimeX
    {
        this( length, 0 );
    }
    public Bytevector( long length, long filler ) throws RuntimeX
    {
        _vector = new byte[ Scut.assertLength( length ) ];

        byte bFiller = Scut.assertByte( filler );

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
            _vector[Scut.assertIndex( idx )] = Scut.assertByte( value );

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
            return 0xff & _vector[Scut.assertIndex( idx )];
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
    public byte[] toJava()
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

    /**
     * Append this with another bytevector into a newly allocated bytevector.
     *
     * @param other The bytevector to append.
     * @return The new bytevector.
     * @throws RuntimeX
     */
    public Bytevector append( Bytevector other )
            throws RuntimeX
    {
        if ( other.size() == 0 )
            return copy();
        if ( size() == 0 )
            return other.copy();

        Bytevector result = new Bytevector(
                Arrays.copyOf(
                        _vector,
                        _vector.length + other._vector.length ) );
        return result.copyFrom(
                _vector.length,
                other,
                0,
                other.size() );
    }

    @Override
    public Bytevector copy()
    {
        return new Bytevector( _vector );
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
                Scut.assertMaxIndex( _vector.length, start );
        var iTo =
                Scut.assertMaxIndex( _vector.length, to );
        if ( start > to )
            throw RuntimeX.mIllegalArgument( "start > to" );

        return new Bytevector(
                Arrays.copyOfRange( _vector, iStart, iTo ) );
    }

    /**
     * Copies the content from source into this bytevector to startIdx.
     *
     * @param source Source bytevector.
     * @param tgtIdx Target index.
     * @param srcIdx The start index in the source vector.
     * @param srcIdx The end index in the source vector.
     * @return This.
     * @throws RuntimeX If this is constant or the data does not fit.
     */
    public Bytevector copyFrom(
            long tgtIdx,
            Bytevector source,
            long srcIdx,
            long endIdx )
        throws RuntimeX
    {
        if ( isConstant() )
            throw RuntimeX.mCannotModifyConstant( this );

        if ( endIdx < srcIdx )
            throw RuntimeX.mIllegalArgument( "endIdx < srcIndex" );

        int iTgtIdx =
                Scut.assertIndex( tgtIdx );
        int iSrcIdx =
                Scut.assertIndex( srcIdx );
        int iEndIdx =
                Scut.assertMaxIndex( source.size(), endIdx );

        int toCopy =
                iEndIdx - iSrcIdx;
        int copyable =
                _vector.length - iTgtIdx;

        if ( toCopy == 0 )
            return this;
        if ( toCopy < 0 )
            throw RuntimeX.mIllegalArgument( "toCopy < 0" );


        // Prevent ioob below.
        if ( copyable < toCopy )
            throw RuntimeX.mIndexOutOfBounds( _vector.length );

        try
        {
            System.arraycopy(
                    source._vector,
                    iSrcIdx,
                    _vector,
                    iTgtIdx,
                    toCopy );
        }
        catch ( ArrayIndexOutOfBoundsException  e )
        {
            throw new InternalError( "Unexpected." );
        }

        return this;
    }

    /**
     * Convert this to a SchemeString.
     *
     * @param startIdx The index of the first byte to be included.
     * @param endIdx The last byte to include.
     * @return A newly allocated string.
     * @throws RuntimeX
     */
    public SchemeString asString( long startIdx, long endIdx )
            throws RuntimeX
    {
        if ( endIdx < startIdx )
            throw RuntimeX.mIllegalArgument( "end < start" );
        var length =
                size();
        var iStart =
                Scut.assertMaxIndex( length, startIdx );
        var iEnd =
                Scut.assertMaxIndex( length, endIdx );

        return SchemeString.make(
                new String(
                        _vector,
                        iStart,
                        iEnd - iStart,
                        StandardCharsets.UTF_8 ) );
    }

    /**
     * @return A stream on the contained bytes.
     */
    public InputStream asStream()
    {
    	return new ByteArrayInputStream( _vector );
    }
}
