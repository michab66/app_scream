/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2022 Michael G. Binz
 */
package de.michab.scream.util;

import java.util.HashSet;

import de.michab.scream.RuntimeX;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.SchemeInteger;

/**
 * Scream utilities.
 *
 * @author micbinz
 */
public class Scut
{
    @FunctionalInterface
    public interface ConsumerX<T> {
        void accept(T t) throws RuntimeX;
    }

    public static long checkProperLength(
            Cons cons,
            long min,
            long max,
            ConsumerX<Long> below,
            ConsumerX<Long> above )
        throws RuntimeX
    {
        if ( ! Cons.isProper( cons ) )
            throw RuntimeX.mExpectedProperList( cons );

        var length = Cons.length( cons );

        if ( length < min )
            below.accept( length );
        else if ( length > max )
            above.accept( length );

        return length;
    }

    public static long checkProperLength(
            Cons cons,
            long expected,
            ConsumerX<Long> fail )
        throws RuntimeX
    {
        return checkProperLength( cons, expected, expected, fail, fail );
    }

    /**
     * Convert an object.  Calls the fail-lambda if the conversion
     * is not possible.
     *
     * @param <T> Conversion target type.
     * @param c Conversion target class.
     * @param v The object to convert.
     * @param fail Called in case of failure.  This can return a T-object
     * which is returned as the result of this operation.  May throw an
     * exception.
     * @return The converted object.
     * @throws RuntimeX
     */
    @SuppressWarnings("unchecked")
    public static <T extends FirstClassObject> T as(
            Class<T> c,
            FirstClassObject v,
            FunctionX<FirstClassObject, T, RuntimeX> fail ) throws RuntimeX
    {
        if ( v == Cons.NIL )
            return (T)v;

        try
        {
            return c.cast(v);
        }
        catch (ClassCastException e) {
            return fail.apply( v );
        }
    }

    public static <T extends FirstClassObject> T as( Class<T> c, FirstClassObject v ) throws RuntimeX
    {
        FunctionX<FirstClassObject, T, RuntimeX> fail = (s) ->
            { throw RuntimeX.mTypeError( c, s.getClass() );  };

        return as(
                c,
                v,
                fail
                );
    }

    public static <T extends FirstClassObject> T asNotNil( Class<T> c, FirstClassObject v )
            throws RuntimeX
    {
        if ( Cons.NIL == v )
            throw RuntimeX.mTypeError( c, null );

        FunctionX<FirstClassObject, T, RuntimeX> fail = (s) ->
            { throw RuntimeX.mTypeError( c, s.getClass() );  };

        return as(
                c,
                v,
                fail
                );
    }

    public static void checkUnique( Cons c ) throws RuntimeX
    {
        var unifier = new HashSet<FirstClassObject>();
        checkUnique( unifier, c );
    }

    public static void checkUnique(
            HashSet<FirstClassObject> unifier,
            Cons c ) throws RuntimeX
    {
        while ( true )
        {
            var car = c.getCar();
            var cdr = c.getCdr();

            if ( ! unifier.add( car ) )
                throw RuntimeX.mDuplicateElement( car );

            if ( Cons.NIL == cdr )
                break;
            if ( cdr instanceof Cons )
            {
                c = (Cons)cdr;
                continue;
            }

            if ( ! unifier.add( cdr ) )
                throw RuntimeX.mDuplicateElement( cdr );
            break;
        }
    }

    public static int assertIndex( long idx ) throws RuntimeX
    {
        if ( idx < 0 || idx >= Integer.MAX_VALUE )
            throw RuntimeX.mRangeExceeded(
                    SchemeInteger.createObject( idx ),
                    "[0.." + Integer.MAX_VALUE + "]" );

        return (int)idx;
    }

    public static <A> int assertMaxIndex( int maximum, long idx ) throws RuntimeX
    {
        if ( idx < 0 || idx > Integer.MAX_VALUE )
            throw RuntimeX.mRangeExceeded(
                    SchemeInteger.createObject( idx ),
                    "[0.." + Integer.MAX_VALUE + "]" );
        if ( idx > maximum )
            throw RuntimeX.mIndexOutOfBounds( idx );

        return (int)idx;
    }

    public static int assertLength( long len ) throws RuntimeX
    {
        if ( len < 0 || len > Integer.MAX_VALUE )
            throw RuntimeX.mRangeExceeded(
                    SchemeInteger.createObject( len ),
                    "[0.." + Integer.MAX_VALUE + "]" );

        return (int)len;
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
