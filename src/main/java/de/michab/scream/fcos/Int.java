/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream.fcos;

import java.util.Arrays;

import de.michab.scream.RuntimeX;

/**
 * Represents the scheme integer data type.
 */
public class Int
    extends Number
{
    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = "integer";

    private final static long CACHE_MIN = -100;
    private final static long CACHE_MAX =  100;
    private static Int[] _cache;

    /**
     * This integer's value.
     */
    private final long _value;

    /**
     * A factory for scheme integers.  Used for limiting generation for well
     * known instances as for 1, 2, ... of this type.
     */
    static public Int createObject( long v )
    {
        return createObject( v, true );
    }
    static public Int createObject( long v, boolean exact )
    {
        if ( ! exact )
            return new Int( v, exact );

        if ( v >= CACHE_MIN && v <= CACHE_MAX )
        {
            int cacheIdx = (int)(v - CACHE_MIN);
            if ( null == _cache[ cacheIdx ] )
                _cache[ cacheIdx ] = new Int( v, exact );
            return _cache[ cacheIdx ];
        }

        return new Int( v, exact );
    }

    /**
     * Make an instance.
     */
    private Int( long v, boolean exact )
    {
        super( exact );

        _value = v;
    }

    /**
     * @see Object#toString
     */
    @Override
    public String toString()
    {
        return "" + _value;
    }

    /**
     * @see Number#asLong
     */
    @Override
    public long asLong()
    {
        return _value;
    }

    /**
     * @see Number#asDouble
     */
    @Override
    public double asDouble()
    {
        return _value;
    }

    /**
     * @see Number#add
     */
    @Override
    public Number add( FirstClassObject other )
            throws RuntimeX
    {
        if ( other instanceof Real )
            return Real.createObject( _value + ((Real)other).asDouble() );
        else if ( other instanceof Int )
            return Int.createObject( _value + ((Int)other).asLong() );

        // Do the needed typecheck.  Since 'other' must be NIL or of a type
        // other than scream.Number this has to fail...
        Operation.checkArgument( 1, Number.class, other );
        // ...or we found an internal error.
        throw RuntimeX.mInternalError( getClass() );
    }

    /**
     * @see Number#subtract
     */
    @Override
    public Number subtract( FirstClassObject other )
            throws RuntimeX
    {
        if ( other instanceof Real )
            return Real.createObject( _value - ((Real)other).asDouble() );
        else if ( other instanceof Int )
            return Int.createObject( _value - ((Int)other).asLong() );

        // Do the needed typecheck.  Since 'other' must be NIL or of a type
        // other than scream.Number this has to fail...
        Operation.checkArgument( 1, Number.class, other );
        // ...or we found an internal error.
        throw RuntimeX.mInternalError( getClass() );
    }

    /**
     * @see Number#multiply
     */
    @Override
    public Number multiply( FirstClassObject other )
            throws RuntimeX
    {
        if ( other instanceof Real )
            return Real.createObject( _value * ((Real)other).asDouble() );
        else if ( other instanceof Int )
            return Int.createObject( _value * ((Int)other).asLong() );

        // Do the needed typecheck.  Since 'other' must be NIL or of a type
        // other than scream.Number this has to fail...
        Operation.checkArgument( 1, Number.class, other );
        // ...or we found an internal error.
        throw RuntimeX.mInternalError( getClass() );
    }

    /**
     * Returns a <code>java.lang.Long</code> instance corresponding to this
     * <code>integer</code.
     *
     * @return An instance of java.lang.Long.
     */
    @Override
    public Long toJava()
    {
        return Long.valueOf( _value );
    }

    @Override
    public boolean equals( Object other )
    {
        try {
            Int o = (Int)other;

            return _value == o._value;
        }
        catch ( Exception e )
        {
            return false;
        }
    }

    @Override
    public int hashCode()
    {
        return Long.valueOf( _value ).hashCode();
    }

    /*
     * Initialize the cache.
     */
    static
    {
        _cache = new Int[ (int)(CACHE_MAX - CACHE_MIN + 1) ];

        Arrays.fill( _cache, Cons.NIL );
    }

    @Override
    public boolean r7rsEqual( Number z ) throws RuntimeX
    {
        return z.isExact() ?
            asLong() == z.asLong() :
            asDouble() == z.asDouble();
    }
    @Override
    public boolean r7rsLessThan( Number z ) throws RuntimeX
    {
        return z.isExact() ?
                asLong() < z.asLong() :
                asDouble() < z.asDouble();
    }
    @Override
    public boolean r7rsGreaterThan( Number z ) throws RuntimeX
    {
        return z.isExact() ?
                asLong() > z.asLong() :
                asDouble() > z.asDouble();
    }
    @Override
    public boolean r7rsLessOrEqualThan( Number z ) throws RuntimeX
    {
        return z.isExact() ?
                asLong() <= z.asLong() :
                asDouble() <= z.asDouble();
    }
    @Override
    public boolean r7rsGreaterOrEqualThan( Number z ) throws RuntimeX
    {
        return z.isExact() ?
                asLong() >= z.asLong() :
                asDouble() >= z.asDouble();
    }

    public boolean r7rsOddQ()
    {
        return (asLong() & 0x1) != 0;
    }
}
