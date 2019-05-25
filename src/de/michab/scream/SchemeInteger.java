/* $Id: SchemeInteger.java 190 2009-07-01 20:53:40Z Michael $
 *
 * Scream / Kernel
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2009 Michael G. Binz
 */
package de.michab.scream;

import java.util.Arrays;



/**
 * Represents the scheme integer data type.
 */
public class SchemeInteger
  extends Number
{
  /**
   * The name of the type as used by error reporting.
   *
   * @see FirstClassObject#getTypename()
   */
  public static final String TYPE_NAME = "integer";



  /*
   * A cache for SchemeIntegers.  Cache size is configurable.
   */
  private final static long CACHE_MIN = -100;
  private final static long CACHE_MAX =  100;
  private static SchemeInteger[] _cache;



  /**
   * This integer's value.
   */
  private final long _value;



  /**
   * A factory for scheme integers.  Used for limiting generation for well
   * known instances as for 1, 2, ... of this type.
   */
  static public SchemeInteger createObject( long v )
  {
    // Check if the value is in the cached interval.
    if ( v >= CACHE_MIN && v <= CACHE_MAX )
    {
      // Yeah.  Map to the cache index...
      int cacheIdx = (int)(v - CACHE_MIN);
      // ...and ensure the value is cached.
      if ( null == _cache[ cacheIdx ] )
        _cache[ cacheIdx ] = new SchemeInteger( v );
      // At last return the cached value.
      return _cache[ cacheIdx ];
    }
    else
      // Is not in the cache interval.
      return new SchemeInteger( v );
  }



  /**
   *
   */
  private SchemeInteger( long v )
  {
    super( false );

    _value = v;
  }



  /**
   * @see FirstClassObject#eqv
   */
  public boolean eqv( FirstClassObject other )
  {
    if ( other instanceof SchemeInteger )
    {
      return asLong() == ((SchemeInteger)other).asLong();
    }
    else if ( other instanceof SchemeDouble )
    {
      return asLong() == ((SchemeDouble)other).asDouble();
    }

    return false;
  }



  /**
   * @see Object#toString
   */
  public String toString()
  {
    return "" + _value;
  }



  /**
   * @see Number#asLong
   */
  public long asLong()
  {
    return _value;
  }



  /**
   * @see Number#asDouble
   */
  public double asDouble()
  {
    return (double)_value;
  }



  /**
   * @see Number#add
   */
  public Number add( FirstClassObject other )
    throws RuntimeX
  {
    if ( other instanceof SchemeDouble )
      return SchemeDouble.createObject( (double)_value + ((SchemeDouble)other).asDouble() );
    else if ( other instanceof SchemeInteger )
      return SchemeInteger.createObject( _value + ((SchemeInteger)other).asLong() );

    // Do the needed typecheck.  Since 'other' must be NIL or of a type
    // other than scream.Number this has to fail...
    Operation.checkArgument( 1, Number.class, other );
    // ...or we found an internal error.
    throw new RuntimeX( "INTERNAL_ERROR", new Object[]{ getClass() } );
  }



  /**
   * @see Number#subtract
   */
  public Number subtract( FirstClassObject other )
    throws RuntimeX
  {
    if ( other instanceof SchemeDouble )
      return SchemeDouble.createObject( (double)_value - ((SchemeDouble)other).asDouble() );
    else if ( other instanceof SchemeInteger )
      return SchemeInteger.createObject( _value - ((SchemeInteger)other).asLong() );

    // Do the needed typecheck.  Since 'other' must be NIL or of a type
    // other than scream.Number this has to fail...
    Operation.checkArgument( 1, Number.class, other );
    // ...or we found an internal error.
    throw new RuntimeX( "INTERNAL_ERROR", new Object[]{ getClass() } );
  }



  /**
   * @see Number#multiply
   */
  public Number multiply( FirstClassObject other )
    throws RuntimeX
  {
    if ( other instanceof SchemeDouble )
      return SchemeDouble.createObject( (double)_value * ((SchemeDouble)other).asDouble() );
    else if ( other instanceof SchemeInteger )
      return SchemeInteger.createObject( _value * ((SchemeInteger)other).asLong() );

    // Do the needed typecheck.  Since 'other' must be NIL or of a type
    // other than scream.Number this has to fail...
    Operation.checkArgument( 1, Number.class, other );
    // ...or we found an internal error.
    throw new RuntimeX( "INTERNAL_ERROR", new Object[]{ getClass() } );
  }



  /**
   * @see Number#divide
   */
  public Number divide( FirstClassObject other )
    throws RuntimeX
  {
    if ( other instanceof SchemeDouble )
      return SchemeDouble.createObject( (double)_value / ((SchemeDouble)other).asDouble() );
    else if ( other instanceof SchemeInteger )
      return SchemeInteger.createObject( _value / ((SchemeInteger)other).asLong() );

    // Do the needed typecheck.  Since 'other' must be NIL or of a type
    // other than scream.Number this has to fail...
    Operation.checkArgument( 1, Number.class, other );
    // ...or we found an internal error.
    throw new RuntimeX( "INTERNAL_ERROR", new Object[]{ getClass() } );
  }



  /**
   * Returns a <code>java.lang.Long</code> instance corresponding to this
   * <code>integer</code.
   *
   * @return An instance of java.lang.Long.
   */
  public Object convertToJava()
  {
    return new Long( _value );
  }



  /*
   * Initialize the cache.
   */
  static
  {
    _cache = new SchemeInteger[ (int)(CACHE_MAX - CACHE_MIN + 1) ];

    Arrays.fill( _cache, Cons.NIL );
  }
}
