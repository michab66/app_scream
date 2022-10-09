/* $Id: SchemeDouble.java 789 2015-01-10 23:13:22Z Michael $
 *
 * Scream / Kernel
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2000 Michael G. Binz
 */
package de.michab.scream;

import org.smack.util.collections.WeakMapWithProducer;




/**
 * Represents the scheme real type.
 */
public class SchemeDouble
extends
de.michab.scream.Number
{
    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#getTypename()
     */
    public static final String TYPE_NAME = "real";



    /**
     * This object's value.
     */
    private final double _value;



    /**
     *
     */
    private static WeakMapWithProducer<Double, SchemeDouble> _flyweights =
            new WeakMapWithProducer<>( c -> new SchemeDouble( c ) );



    /**
     * A factory for scheme doubles.  Used for limiting generation for well
     * known instances as for 1.0, 2.0, ... of this type.
     */
    static public SchemeDouble createObject( double v )
    {
        return _flyweights.get( v );
    }



    /**
     * Create a new SchemeDouble.  This is only accessible via createObject to
     * allow for caching well known instances like 1.0 etc.
     */
    private SchemeDouble( double v )
    {
        super( false );

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
            throw new RuntimeX( "INTERNAL_ERROR", new Object[]{ getClass() } );
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
            throw new RuntimeX( "INTERNAL_ERROR", new Object[]{ getClass() } );
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
            throw new RuntimeX( "INTERNAL_ERROR", new Object[]{ getClass() } );
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
            throw new RuntimeX( "INTERNAL_ERROR", new Object[]{ getClass() } );
        }
    }



    /**
     * Converts this <code>SchemeDouble</code> instance into a
     * <code>java.lang.Double</code>.
     */
    @Override
    public Object toJava()
    {
        return Double.valueOf( _value );
    }
}
