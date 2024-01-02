/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2022-2024 Michael G. Binz
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
            { throw RuntimeX.mTypeError( c, s );  };

        return as(
                c,
                v,
                fail );
    }

    public static <T extends FirstClassObject> T asNotNil( Class<T> c, FirstClassObject v )
            throws RuntimeX
    {
        if ( Cons.NIL == v )
            throw RuntimeX.mTypeError( c, Cons.NIL );

        return as( c, v );
    }

    /**
     * Check if the passed list contains no duplicate elements. Supports
     * improper lists.
     *
     * @param cons The list to check.
     * @return The passed list.
     * @throws RuntimeX mDuplicateElement if duplicate elements were found.
     */
    public static Cons assertUnique( Cons cons ) throws RuntimeX
    {
        return assertUnique(
                new HashSet<FirstClassObject>(),
                cons );
    }

    /**
     * Check if the passed list contains no duplicate elements. Supports
     * improper lists.  This form can be used if uniqueness has to be checked
     * across several lists by passing the same unifier in each call.
     *
     * @param unifier A has
     * @param cons The list to check.
     * @return The passed list.
     *
     * @throws RuntimeX mDuplicateElement if duplicate elements were found.
     */
    public static Cons assertUnique(
            HashSet<FirstClassObject> unifier,
            Cons cons )
        throws RuntimeX
    {
        final Cons result = cons;

        while ( true )
        {
            var car = cons.getCar();
            var cdr = cons.getCdr();

            if ( ! unifier.add( car ) )
                throw RuntimeX.mDuplicateElement( car );

            if ( Cons.NIL == cdr )
                break;
            if ( cdr instanceof Cons )
            {
                cons = (Cons)cdr;
                continue;
            }

            if ( ! unifier.add( cdr ) )
                throw RuntimeX.mDuplicateElement( cdr );
            break;
        }

        return result;
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

    /**
     * Ensure that the passed list contains only elements of the
     * passed type.
     *
     * @param list The list to check.  Must be proper.
     * @param type The expected type of the elements.
     * @throws RuntimeX If the list contains other types than {@code type}.
     */
    public static void assertHomogeneous(
            Cons list,
            Class<? extends FirstClassObject> type )
        throws RuntimeX
    {
        if ( ! Cons.isProper( list ) )
            throw RuntimeX.mExpectedProperList();

        for ( var c : list )
            as( type, c );
    }
}
