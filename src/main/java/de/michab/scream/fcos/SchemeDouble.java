/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import de.michab.scream.RuntimeX;

/**
 * Represents the scheme real type.
 */
public class SchemeDouble extends
    de.michab.scream.fcos.Number
{
    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = "real";

    /**
     * This object's value.
     */
    private final double _value;

    /**
     * A factory for scheme doubles.
     */
    static public SchemeDouble createObject( double v )
    {
        return new SchemeDouble( v, false );
    }
    static public SchemeDouble createObject( double v, boolean exact )
    {
        return new SchemeDouble( v, exact );
    }

    /**
     * Create a new SchemeDouble.
     */
    private SchemeDouble( double v, boolean exact )
    {
        super( exact );

        _value = v;
    }

    /**
     * See FirstClassObject#eqv
     */
    @Override
    public boolean eqv( FirstClassObject other )
    {
        boolean result;

        try
        {
            result = _value == ((Number)other).asDouble();
        }
        // This could catch either a ClassCast or NullPointer exception.  Is the
        // quickest way to let the VM check for all error conditions.
        catch ( Exception e )
        {
            result = false;
        }

        return result;
    }

    /**
     * See java.lang.Object#toString
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
        return (long)_value;
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
        try
        {
            return createObject( _value + ((Number)other).asDouble() );
        }
        // Two exceptions are possible from the code above:  For the failed class
        // cast or because of a NIL reference.  The handling for both is the same.
        catch ( Exception e )
        {
            // Do the needed typecheck.  Since 'other' must be NIL or of a type
            // other than scream.Number this has to fail...
            Operation.checkArgument( 1, Number.class, other );
            // ...or we found an internal error.
            throw RuntimeX.mInternalError( getClass() );
        }
    }

    /**
     * @see Number#subtract
     */
    @Override
    public Number subtract( FirstClassObject other )
            throws RuntimeX
    {
        try
        {
            return createObject( _value - ((Number)other).asDouble() );
        }
        // Two exceptions are possible from the code above:  For the failed class
        // cast or because of a NIL reference.  The handling for both is the same.
        catch ( Exception e )
        {
            // Do the needed typecheck.  Since 'other' must be NIL or of a type
            // other than scream.Number this has to fail...
            Operation.checkArgument( 1, Number.class, other );
            // ...or we found an internal error.
            throw RuntimeX.mInternalError( getClass() );
        }
    }

    /**
     * @see Number#multiply
     */
    @Override
    public Number multiply( FirstClassObject other )
            throws RuntimeX
    {
        try
        {
            return createObject( _value * ((Number)other).asDouble() );
        }
        // Two exceptions are possible from the code above:  For the failed class
        // cast or because of a NIL reference.  The handling for both is the same.
        catch ( Exception e )
        {
            // Do the needed typecheck.  Since 'other' must be NIL or of a type
            // other than scream.Number this has to fail...
            Operation.checkArgument( 1, Number.class, other );
            // ...or we found an internal error.
            throw RuntimeX.mInternalError( getClass() );
        }
    }

    /**
     * @see Number#divide
     */
    @Override
    public Number divide( FirstClassObject other )
            throws RuntimeX
    {
        try
        {
            return createObject( _value / ((Number)other).asDouble() );
        }
        // Two exceptions are possible from the code above:  For the failed class
        // cast or because of a NIL reference.  The handling for both is the same.
        catch ( Exception e )
        {
            // Do the needed typecheck.  Since 'other' must be NIL or of a type
            // other than scream.Number this has to fail...
            Operation.checkArgument( 1, Number.class, other );
            // ...or we found an internal error.
            throw RuntimeX.mInternalError( getClass() );
        }
    }

    /**
     * Converts this <code>SchemeDouble</code> instance into a
     * <code>java.lang.Double</code>.
     */
    @Override
    public Double toJava()
    {
        return Double.valueOf( _value );
    }

    @Override
    public boolean r7rsEqual( Number z ) throws RuntimeX
    {
        return asDouble() == z.asDouble();
    }
    @Override
    public boolean r7rsLessThan( Number z ) throws RuntimeX
    {
        return asDouble() < z.asDouble();
    }
    @Override
    public boolean r7rsGreaterThan( Number z ) throws RuntimeX
    {
        return asDouble() > z.asDouble();
    }
    @Override
    public boolean r7rsLessOrEqualThan( Number z ) throws RuntimeX
    {
        return asDouble() <= z.asDouble();
    }
    @Override
    public boolean r7rsGreaterOrEqualThan( Number z ) throws RuntimeX
    {
        return asDouble() >= z.asDouble();
    }
}
