/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream.fcos;

import java.util.Arrays;

import de.michab.scream.RuntimeX;

/**
 * Implements the Scheme vector type.
 */
public class Vector
    extends FirstClassObject
{
    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = "vector";

    /**
     * The array that holds the vector elements.
     */
    private final FirstClassObject[] _theArray;

    /**
     * Create an array of the specified size.  The array slots are initialized
     * to NIL (the empty list).
     *
     * @param size Size of the array to be created.
     */
    public Vector( long size )
    {
        this( size, Cons.NIL );
    }

    /**
     * Creates an array of the specified size and initializes its slots
     * to the specified value.  Note that each slot is set to the same
     * initial object, so that the elements are equal in the sense of
     * the Scheme predicate eqv?.
     *
     * @param size The size of the array to be created.
     * @param initialiser The initial value for the array slots.
     */
    public Vector( long size, FirstClassObject initialiser )
    {
        _theArray = new FirstClassObject[ (int)size ];
        Arrays.fill( _theArray, initialiser );
    }

    /**
     * Create a Scheme vector from the passed array.  The array is copied.
     *
     * @param array The array holding the initial contents of the resulting
     *              vector.
     * @param copyArray In case this is false, the passed array won't be copied
     *                  but used as is for the internal representation of the
     *                  vector.
     */
    public Vector( FirstClassObject[] array, boolean copyArray )
    {
        if ( copyArray )
        {
            _theArray = Arrays.copyOf( array, array.length);
        }
        else
            _theArray = array;
    }

    /**
     * Create a Scheme vector from the passed elements.
     *
     * @param array The initial contents of the resulting vector.
     */
    public Vector( FirstClassObject ... array )
    {
        this( array, true );
    }

    /**
     * Sets an array slot to the specified value.
     *
     * @param idx The index of the slot to set.
     * @param so The value to set.
     * @throws RuntimeX in case the array index is out of bounds.
     */
    public void set( long idx, FirstClassObject so )
        throws RuntimeX
    {
        if ( isConstant() )
            throw RuntimeX.mCannotModifyConstant( this );

        try
        {
            _theArray[ (int)idx ] = so;
        }
        catch ( ArrayIndexOutOfBoundsException e )
        {
            throw RuntimeX.mIndexOutOfBounds( idx );
        }
    }

    /**
     * Access the value of an array slot.
     *
     * @param idx The index of the slot to access.
     * @return The slot's value.
     */
    public FirstClassObject get( long idx )
        throws RuntimeX
    {
        try
        {
            return _theArray[ (int)idx ];
        }
        catch ( IndexOutOfBoundsException e )
        {
            throw RuntimeX.mIndexOutOfBounds( idx );
        }
    }

    /**
     * Get the size of the array.
     *
     * @return The size of the array.
     */
    public long size()
    {
        return _theArray.length;
    }

    /**
     * Fill all vector slots with the passed value.  The value is not copied.
     *
     * @param filler The value to be used for filling the vector.
     * @throws RuntimeX
     */
    public void fill( FirstClassObject filler ) throws RuntimeX
    {
        if ( isConstant() )
            throw RuntimeX.mCannotModifyConstant( this );

        Arrays.fill( _theArray, filler );
    }

    /**
     * The implementation of the scheme equal? procedure.  This is the least
     * efficient one since lists and arrays are deep compared. For other types
     * eqv? is used.
     *
     * @param other The FirstClassObject to compare with.
     */
    @Override
    public boolean equal( FirstClassObject other )
    {
        if ( other instanceof Vector &&
                _theArray.length == ((Vector)other)._theArray.length )
        {
            Vector otherVector = (Vector)other;

            for ( int i = 0 ; i < _theArray.length ; i++ )
            {
                boolean idxeqv =
                        equal( _theArray[i], otherVector._theArray[i] );

                if ( ! idxeqv )
                    return idxeqv;
            }

            return true;
        }

        return false;
    }

    /**
     * @see FirstClassObject#toString
     */
    @Override
    public String toString()
    {
        StringBuilder result = new StringBuilder( "#(" );

        // Add one vector element after the other.
        for ( int i = 0 ; i < _theArray.length ; i++ )
        {
            result.append( toString( _theArray[i] ) );
            result.append( ' ' );
        }

        // Remove the space at the end added in the above loop.
        if ( _theArray.length > 0 )
            result.setLength( result.length() -1 );

        return result.append( ')' ).toString();
    }

    @Override
    public Vector copy()
    {
        return new Vector( _theArray );
    }

    /**
     * Converts the vector to an array of {@code java.lang.Object}s holding
     * recursively converted entries.
     * @throws RuntimeX
     */
    @Override
    public Object[] toJava() throws RuntimeX
    {
        Object[] result = new Object[ _theArray.length ];

        for ( int i = 0 ; i < result.length ; i++ )
            result[i] = _theArray[i].toJava();

        return result;
    }

    /**
     * @return An array of fcos holding the vector's elements.
     */
    public FirstClassObject[] asArray()
    {
        return Arrays.copyOf( _theArray, _theArray.length );
    }
}
