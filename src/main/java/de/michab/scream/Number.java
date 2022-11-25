/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream;

import de.michab.scream.ScreamException.Code;

/**
 * The base class for Scream's numeric types.
 */
public abstract class Number
  extends FirstClassObject
{
  /**
   * The name of the type as used by error reporting.
   *
   * @see FirstClassObject#typename()
   */
  public static final String TYPE_NAME = "number";



  /**
   * Type save enumeration class.
   */
  private static enum ComparisonType
  {
    EQ, LT, LET, GT, GET
  };



  public static final ComparisonType EQ = ComparisonType.EQ;
  public static final ComparisonType LT = ComparisonType.LT;
  public static final ComparisonType LET = ComparisonType.LET;
  public static final ComparisonType GT = ComparisonType.GT;
  public static final ComparisonType GET = ComparisonType.GET;



  /**
   * The exactness flag as described in the scheme standard.  Not really
   * implemented.  The flag is always false.
   */
  private final boolean _isExact;



  /**
   * A predicate for the exactness flag as specified by the scheme standard.
   *
   * @return Whether this represents an exact or inexact number.
   */
  final public boolean isExact()
  {
    return _isExact;
  }



  /**
   * Create a number instance with a given exactness.
   *
   * @param isExact Specifies if the number is exact or inexact.
   */
  Number( boolean isExact )
  {
    _isExact = isExact;
    setConstant( true );
  }



  /**
   * Default constructor.  Since this is an abstract class this is only
   * implicitly called from an derived class implementing <code>Number</code>.
   */
  Number()
  {
    this( false );
  }



  /**
   * Returns this number's value as long.
   *
   * @return This number's value as long.
   */
  public abstract long asLong();



  /**
   * Returns this number's value as double.
   *
   * @return This number's value as double.
   */
  public abstract double asDouble();



  /**
   * Computes this plus the argument.
   *
   * @param other The corresponding number to add.
   * @return The sum of this and other.
   * @throws RuntimeX In case an error occurred.
   */
  public abstract Number add( FirstClassObject other )
    throws RuntimeX;



  /**
   * Computes this - other.
   *
   * @param other The corresponding number to subtract.
   * @return The difference between this and other.
   * @throws RuntimeX In case an error occurred.
   */
  public abstract Number subtract( FirstClassObject other )
    throws RuntimeX;



  /**
   * Computes this * other.
   * @param other The corresponding number to multiply.
   * @return The product of this and other.
   * @throws RuntimeX In case an error occurred.
   */
  public abstract Number multiply( FirstClassObject other )
    throws RuntimeX;



  /**
   * Computes this / other.
   * @param other The corresponding number to divide.
   * @return The quotient of this and other.
   * @throws RuntimeX In case an error occurred.
   */
  public abstract Number divide( FirstClassObject other )
    throws RuntimeX;



  /**
   * Implements the comparison for the < <= = >= > operations.
   *
   * @param args The array of arguments for the comparison.
   * @param operation The operation flag, one of EQ LT LET GT GET.
   * @return The result of the comparison.
   * @throws RuntimeX In case a wrong operation argument was given.
   */
  public static FirstClassObject
    compare( FirstClassObject[] args, ComparisonType operation )
  throws
    RuntimeX
  {
    // Ensure valid non-NIL arguments.
    catchNil( args );

    // Comparisons are based on doubles in this method.

    // Create an array that holds the values for the further comparison.
    double[] dargs = new double[ args.length ];
    // Now init this array and implicitly check types.
    for ( int i = 0 ; i < args.length ; i++ )
    {
      try
      {
        dargs[i] = ((Number)args[i]).asDouble();
      }
      catch ( Exception e )
      {
        Operation.checkArgument( 1, Number.class, args[i] );
      }
    }

    // Easy going now.  Check if the entries in the array compare according
    // to the operations argument.
    boolean result = false;
    for ( int i = 0 ; i < args.length -1 ; i++ )
    {
      switch ( operation )
      {
        case EQ:
          result = dargs[i] == dargs[i+1];
          break;
        case LT:
          result = dargs[i] < dargs[i+1];
          break;
        case LET:
          result = dargs[i] <= dargs[i+1];
          break;
        case GT:
          result = dargs[i] > dargs[i+1];
          break;
        case GET:
          result = dargs[i] >= dargs[i+1];
          break;
        default:
          throw new RuntimeX( Code.INTERNAL_ERROR, Number.class );
      }

      // Shortcut evaluation.
      if ( !result )
        break;
    }

    return SchemeBoolean.createObject( result );
  }



  /**
   * Used for catching NIL values in number lists.
   *
   * @param list The list of objects to be checked.
   * @throws RuntimeX In case the list contains NIL.
   */
  static void catchNil( FirstClassObject[] list )
    throws RuntimeX
  {
    for ( int i = list.length -1 ; i >= 0 ; i-- )
      Operation.checkArgument( i, Number.class, list[i] );
  }



  /**
   * (+ ...
   */
  static private Procedure addProc = new Procedure( "+" )
  {
    @Override
    public FirstClassObject apply( FirstClassObject[] args )
      throws RuntimeX
    {
      Number result = SchemeInteger.createObject( 0 );
      // Note: Types will be checked in add.
      for ( int i = 0 ; i < args.length ; i++ )
        result = result.add( args[i] );

      return result;
    }
  };



  /**
   * (- ...
   */
  static private Procedure subtractProc = new Procedure( "-" )
  {
    @Override
    public FirstClassObject apply( FirstClassObject[] args )
      throws RuntimeX
    {
      Number result = SchemeInteger.createObject( 0 );

      if ( 1 == args.length )
          result = result.subtract( args[0] );
      else if ( args.length > 1 )
      {
        // Prevent the need for typechecking.
        result = result.add( args[ 0 ] );
        // Note: Types will be checked in subtract.
        for ( int i = 1 ; i < args.length ; i++ )
          result = result.subtract( args[i] );
      }

      return result;
    }
  };



  /**
   * (* ...
   */
  static private Procedure multiplyProc = new Procedure( "*" )
  {
    // TODO NIL handling is bad.
    @Override
    public FirstClassObject apply( FirstClassObject[] args )
      throws RuntimeX
    {
      Number result = null;

      try
      {
        catchNil( args );

        if ( 0 == args.length )
          result = SchemeInteger.createObject( 1 );
        else if ( 1 == args.length )
          result = (Number)args[ 0 ];
        else
        {
          result = (Number)args[ 0 ];
          // Note: Types will be checked in multiply.
          for ( int i = 1 ; i < args.length ; i++ )
            result = result.multiply( args[i] );
        }
      }
      catch ( ClassCastException e )
      {
        // Arg 0 was no number somewhere above.  Perform explicit type check
        // and trigger error reporting.
        checkArgument( 1, Number.class, args[0] );
      }

      return result;
    }
  };



  /**
   * (/ ...
   */
  static private Procedure divideProc = new Procedure( "/" )
  {
    @Override
    public FirstClassObject apply( FirstClassObject[] args )
      throws RuntimeX
    {
      Number result = null;

      try
      {
        catchNil( args );
        // In case we have no argument...
        if ( 0 == args.length )
          // ...return 1.
          result = SchemeInteger.createObject( 1 );
        else if ( 1 == args.length )
          // ...return the inverse.
          result = SchemeDouble.createObject( 1.0 ).divide( args[0] );
        else
        {
          result = (Number)args[ 0 ];
          // Note: Types will be checked in divide.
          for ( int i = 1 ; i < args.length ; i++ )
            result = result.divide( args[i] );
        }
      }
      catch ( ClassCastException e )
      {
        // Arg 0 was no number somewhere above.  So do the explicit type check
        // and trigger error handling.
        checkArgument( 1, Number.class, args[0] );
      }
      catch ( ArithmeticException e )
      {
        throw new RuntimeX( Code.DIVISION_BY_ZERO );
      }

      return result;
    }
  };



  /**
   * Number operations setup.
   *
   * @param tle The toplevel-environment to extend.
   * @return The extended environment.
   */
  public static Environment extendTopLevelEnvironment( Environment tle )
  {
    tle.setPrimitive( addProc );
    tle.setPrimitive( subtractProc );
    tle.setPrimitive( multiplyProc );
    tle.setPrimitive( divideProc );

    return tle;
  }
}
