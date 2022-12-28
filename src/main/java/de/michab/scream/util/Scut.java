/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 2022 Michael G. Binz
 */
package de.michab.scream.util;

import java.util.HashSet;

import de.michab.scream.Cons;
import de.michab.scream.Continuation;
import de.michab.scream.Continuation.Cont;
import de.michab.scream.Continuation.Thunk;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamException;
import de.michab.scream.ScreamException.Code;
import de.michab.scream.Symbol;
import urschleim.Holder;

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
            throw mExpectedProperList( cons );

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
            FunctionX<FirstClassObject, T> fail ) throws RuntimeX
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
        FunctionX<FirstClassObject, T> fail = (s) ->
            { throw mTypeError( c, s.getClass() );  };

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
            throw mTypeError( c, Cons.NIL );

        FunctionX<FirstClassObject, T> fail = (s) ->
            { throw mTypeError( c, s.getClass() );  };

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
                throw mDuplicateElement( car );

            if ( Cons.NIL == cdr )
                break;
            if ( cdr instanceof Cons )
            {
                c = (Cons)cdr;
                continue;
            }

            if ( ! unifier.add( cdr ) )
                throw mDuplicateElement( cdr );
            break;
        }
    }

//    EXPECTED_PROPER_LIST = \
//            6 : Expected proper list.
//            EXPECTED_PROPER_LIST_1 = \
//            6 : Expected proper list.  Received {0}
    public static RuntimeX mExpectedProperList(
            FirstClassObject actual )
                    throws RuntimeX
    {
        return new RuntimeX(
                Code.EXPECTED_PROPER_LIST,
                FirstClassObject.toString( actual ) );
    }

    //  #
    //  # 0: Name of expected type
    //  # 1: Name of actual type
    //  # 2: Optional: Position of wrong parameter in a parameter list.
    //  #
    //  TYPE_ERROR_2 = \
    //  11 : Argument has wrong type.  Expected {0} but found {1}.
    //  TYPE_ERROR_3 = \
    //  11 : Argument {2} has wrong type.  Expected {0} but found {1}.
    public static  <T1 extends FirstClassObject, T2 extends FirstClassObject>
    RuntimeX mTypeError(
            Class<T1> expected,
            Class<T2> actual )
                    throws RuntimeX
    {
        return new RuntimeX(
                Code.TYPE_ERROR,
                FirstClassObject.typename( expected ),
                FirstClassObject.typename( actual ) );
    }
    public static  <T1 extends FirstClassObject, T2 extends FirstClassObject>
    RuntimeX mTypeError(
            Class<T1> expected,
            FirstClassObject actual )
                    throws RuntimeX
    {
        return new RuntimeX(
                Code.TYPE_ERROR,
                FirstClassObject.typename( expected ),
                FirstClassObject.toString( actual ) );
    }
    public static  <T1 extends FirstClassObject, T2 extends FirstClassObject>
    RuntimeX mTypeError(
            Class<T1> expected,
            Class<T2> actual,
            int position )
                    throws RuntimeX
    {
        return new RuntimeX(
                Code.TYPE_ERROR,
                FirstClassObject.typename( expected ),
                FirstClassObject.typename( actual ),
                position );
    }

//    #
//    # Used in a number of syntax implementations.
//    #
//    # arg 0: The name of the syntax.
//    # arg 1: The wrong binding
//    #
//    BAD_BINDING_2 = \
//    16 : Bad binding in {0} syntax: {1}
    public static  <T1 extends FirstClassObject>
    RuntimeX mBadBinding(
            Symbol syntaxName,
            Cons binding )
                    throws RuntimeX
    {
        return new RuntimeX(
                Code.BAD_BINDING,
                FirstClassObject.toString( syntaxName ),
                FirstClassObject.toString( binding ) );
    }

    //    BAD_CLAUSE_1 = \
    //            17 : Bad clause: {0}
    public static  <T1 extends FirstClassObject>
    RuntimeX mBadClause(
            FirstClassObject clause )
                    throws RuntimeX
    {
        return new RuntimeX(
                Code.BAD_CLAUSE,
                FirstClassObject.toString( clause ) );
    }

    //  #
    //  # A list contained a duplicate element.  Used in case-syntax.
    //  #
    //  DUPLICATE_ELEMENT_1 = \
    //  46 : Duplicate element : {0}
    public static  <T1 extends FirstClassObject, T2 extends FirstClassObject>
    RuntimeX mDuplicateElement(
            FirstClassObject duplicate )
                    throws RuntimeX
    {
        return new RuntimeX(
                Code.DUPLICATE_ELEMENT,
                FirstClassObject.toString( duplicate ) );
    }

    @FunctionalInterface
    public interface ToStackOp {
        Thunk call( Environment e, Cont<FirstClassObject> c )
            throws RuntimeX;
    }

    public static FirstClassObject toStack( Environment e, ToStackOp op )
        throws RuntimeX
    {
        Holder<FirstClassObject> r =
                new Holder<FirstClassObject>( Cons.NIL );
        Holder<ScreamException> error =
                new Holder<>( null );

        Continuation.trampoline(
                op.call( e,
                        Continuation.endCall( s -> r.set( s ) ) ),
                s -> error.set( s ) );

        if ( error.get() != null )
            throw (RuntimeX)error.get();

        return r.get();
    }
}
