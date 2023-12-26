/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import de.michab.scream.RuntimeX;
import de.michab.scream.Scream.Cont;
import de.michab.scream.pops.Primitives;
import de.michab.scream.util.Continuation.Thunk;
import de.michab.scream.util.Scut;

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
        setConstant();
    }

    /**
     * Default constructor.  Since this is an abstract class this is only
     * implicitly called from an derived class implementing {@code Number}.
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


    @FunctionalInterface
    interface ArithmeticOperation
    {
        Thunk perform(
                Environment e,
                Cons list,
                long listLength,
                Cont<FirstClassObject> c ) throws RuntimeX;
    }

    /**
     * Computes this plus the argument.
     *
     * @param other The corresponding number to add.
     * @return The sum of this and other.
     * @throws RuntimeX In case an error occurred.
     */
    public abstract Number add( FirstClassObject other )
            throws RuntimeX;

    private static Thunk _add( Environment e, Number total, Cons rest, Cont<FirstClassObject> c )
            throws RuntimeX
    {
        if ( rest == Cons.NIL )
            return c.accept( total );

        var current = Scut.as( Number.class, rest.getCar() );

        return _add(
                e,
                total.add( current ),
                Scut.as( Cons.class, rest.getCdr() ),
                c );
    }

    // r7rs p. 36
    private static Thunk _x_add(
            Environment e,
            Cons list,
            long listLength,
            Cont<FirstClassObject> c )
    {
        var zero = SchemeInteger.createObject( 0 );

        if ( listLength == 0 )
            return () -> Primitives._x_quote( e, zero, c );

            return () -> _add(
                    e,
                    zero,
                    list,
                    c );
    }

    /**
     * Computes this - other.
     *
     * @param other The corresponding number to subtract.
     * @return The difference between this and other.
     * @throws RuntimeX In case an error occurred.
     */
    public abstract Number subtract( FirstClassObject other )
            throws RuntimeX;

    private static Thunk _subtract( Environment e, Number total, Cons rest, Cont<FirstClassObject> c )
            throws RuntimeX
    {
        if ( rest == Cons.NIL )
            return c.accept( total );

        var current = Scut.as( Number.class, rest.getCar() );

        return _subtract(
                e,
                total.subtract( current ),
                Scut.as( Cons.class, rest.getCdr() ),
                c );
    }

    private static Thunk _x_subtract(
            Environment e,
            Cons list,
            long listLength,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        if ( listLength == 1 )
            return () -> _subtract(
                    e,
                    SchemeInteger.createObject( 0 ),
                    list,
                    c );

            return () -> _subtract(
                    e,
                    Scut.as( Number.class, list.getCar() ),
                    Scut.as( Cons.class, list.getCdr() ),
                    c );
    }

    /**
     * Computes this * other.
     * @param other The corresponding number to multiply.
     * @return The product of this and other.
     * @throws RuntimeX In case an error occurred.
     */
    public abstract Number multiply( FirstClassObject other )
            throws RuntimeX;

    private static Thunk _multiply( Environment e, Number total, Cons rest, Cont<FirstClassObject> c )
            throws RuntimeX
    {
        if ( rest == Cons.NIL )
            return c.accept( total );

        var current = Scut.as( Number.class, rest.getCar() );

        return _multiply(
                e,
                total.multiply( current ),
                Scut.as( Cons.class, rest.getCdr() ),
                c );
    }

    private static Thunk _x_multiply(
            Environment e,
            Cons list,
            long listLength,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        var one = SchemeInteger.createObject( 1 );

        if ( listLength == 0 )
            return () -> Primitives._x_quote( e, one, c );

            return () -> _multiply(
                    e,
                    one,
                    list,
                    c );
    }

    /**
     * Computes this / other.
     * @param other The corresponding number to divide.
     * @return The quotient of this and other.
     * @throws RuntimeX In case an error occurred.
     */
    public abstract Number divide( FirstClassObject other )
            throws RuntimeX;

    private static Thunk _divide( Environment e, Number total, Cons rest, Cont<FirstClassObject> c )
            throws RuntimeX
    {
        if ( rest == Cons.NIL )
            return c.accept( total );

        var current = Scut.as( Number.class, rest.getCar() );

        try {
            return _divide(
                    e,
                    total.divide( current ),
                    Scut.as( Cons.class, rest.getCdr() ),
                    c );
        }
        catch ( ArithmeticException aex )
        {
            throw RuntimeX.mDivisionByZero();
        }
    }

    private static Thunk _x_divide(
            Environment e,
            Cons list,
            long listLength,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        if ( listLength == 1 )
            return () -> _divide(
                    e,
                    SchemeInteger.createObject( 1 ),
                    list,
                    c );

            return () -> _divide(
                    e,
                    Scut.as( Number.class, list.getCar() ),
                    Scut.as( Cons.class, list.getCdr() ),
                    c );
    }


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
                throw RuntimeX.mInternalError( Number.class.getName() );
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

    private static Thunk doArithmetic(
            ArithmeticOperation opr,
            Environment e,
            Cons args,
            long argsLength,
            Cont<FirstClassObject> c )
        throws RuntimeX
    {
        return opr.perform(
                e,
                args,
                argsLength,
                c );
    }

    /**
     * (+ ...
     */
    static private Procedure addProc( Environment e )
    {
        return new Procedure( "+" )
        {
            @Override
            protected Thunk __executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                    throws RuntimeX
            {
                var len = checkArgumentCount( 0, Integer.MAX_VALUE, args );

                return doArithmetic(
                        Number::_x_add,
                        e,
                        args,
                        len,
                        c );
            }
        }.setClosure( e );
    }

    /**
     * (- ...
     */
    static private Procedure subtractProc( Environment e )
    {
        return new Procedure( "-" )
        {
            @Override
            protected Thunk __executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                    throws RuntimeX
            {
                var len = checkArgumentCount( 1, Integer.MAX_VALUE, args );

                return doArithmetic(
                        Number::_x_subtract,
                        e,
                        args,
                        len,
                        c );
            }
        }.setClosure( e );
    }

    /**
     * (* ...
     */
    static private Procedure multiplyProc( Environment e )
    {
        return new Procedure( "*" )
        {
            @Override
            protected Thunk __executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                    throws RuntimeX
            {
                var len = checkArgumentCount( 0, Integer.MAX_VALUE, args );

                return doArithmetic(
                        Number::_x_multiply,
                        e,
                        args,
                        len,
                        c );
            }

        }.setClosure( e );
    }

    /**
     * (/ ...
     */
    static private Procedure divideProc( Environment e ) {
        return new Procedure( "/" )
        {
            @Override
            protected Thunk __executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                    throws RuntimeX
            {
                var len = checkArgumentCount( 1, Integer.MAX_VALUE, args );

                return doArithmetic(
                        Number::_x_divide,
                        e,
                        args,
                        len,
                        c );
            }
        }.setClosure( e );
    }

    /**
     * Number operations setup.
     *
     * @param tle The toplevel-environment to extend.
     * @return The extended environment.
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
            throws RuntimeX
    {
        tle.setPrimitive( addProc( tle ) );
        tle.setPrimitive( subtractProc( tle ) );
        tle.setPrimitive( multiplyProc( tle ) );
        tle.setPrimitive( divideProc( tle ) );

        return tle;
    }
}
