/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;
import org.smack.util.Holder;

import de.michab.scream.RuntimeX;
import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.SchemeInteger;
import de.michab.scream.util.Continuation.Cont;
import de.michab.scream.util.Continuation.Thunk;

// TODO micbinz move to util tests.
public class ContinuationTest extends ScreamBaseTest
{
    @SuppressWarnings("serial")
    static class TotalX extends Exception
    {
        public final int _total;

        public TotalX( int a, int b )
        {
            _total = a + b;
        }
    }

    private Thunk add( int a, int b, Cont<Integer> c )
    {
        return () -> c.accept( a + b );
    }

    private Thunk addd( int a, int b, Cont<Integer> c )
    {
        return () -> () -> c.accept( a + b );
    }

    private Thunk addx( int a, int b, Cont<Integer> c )
    {
        return () -> {
            throw new ArithmeticException( "addx" );
        };
    }

    @Test
    public void basic() throws Exception
    {
        Continuation<Integer, Exception> continuation =
                new Continuation<>( Exception.class );

        continuation.thunkCount(0);

        int result = continuation.toStack(
                cont -> add(3,4,cont) );

        assertEquals( 1, continuation.thunkCount() );
        assertEquals( 7, result );
    }

    @Test
    public void basic2() throws Exception
    {
        Continuation<Integer, Exception> continuation =
                new Continuation<>( Exception.class );

        continuation.thunkCount(0);

        int result = continuation.toStack(
                cont-> addd(3,4,cont) );

        assertEquals( 2, continuation.thunkCount() );
        assertEquals( 7, result );
    }

    @Test
    public void testSuccess() throws Exception
    {
        Continuation<Integer, Exception> continuation =
                new Continuation<>( Exception.class );

        int result = continuation.toStack(
                cont-> add(3,4,cont),
                null );

        assertEquals( 7, result );
    }

    @Test
    public void testException()
    {
        Continuation<Integer, Exception> continuation =
                new Continuation<>( Exception.class );
        try
        {
            @SuppressWarnings("unused")
            int result = continuation.toStack(
                    cont-> addx(3,4,cont) );
            fail();
        }
        catch ( ArithmeticException e )
        {
            // Expected.
        }
        catch ( Exception e )
        {
            fail();
        }
    }

    private Thunk addWithTotalX( int a, int b, Cont<Integer> c) throws TotalX
    {
        return () -> {
            throw new TotalX( 3, 4 );
            };
    }

    @Test
    public void testException2() throws Exception
    {
        Continuation<Integer, TotalX> continuation =
                new Continuation<>( TotalX.class );

        Holder<TotalX> aeh =
                new Holder<>();

        Cont<TotalX> handler =
                ae -> {
                    aeh.set( ae );
                    return null;
                };

        Integer result = continuation.toStack(
                cont -> addWithTotalX( 3, 4, cont ),
                handler );

        assertEquals( null, result );
        assertEquals( 7, aeh.get()._total );
    }

    private Thunk addFco( SchemeInteger si1, SchemeInteger si2, Cont<FirstClassObject> cont )
    {
        return () -> cont.accept( si1.add( si2 ) );
    }

    @Test
    public void testFcoFunc() throws Exception
    {
        Continuation<FirstClassObject, RuntimeX> cont = new Continuation<>( RuntimeX.class );

        var fco = cont.toStack( continuation -> addFco( i1, i2, continuation ) );

        assertEqualq( i3, fco );
    }

    private Thunk throwRtx( SchemeInteger si1, SchemeInteger si2, Cont<FirstClassObject> cont )
            throws RuntimeX
    {
        return () -> { throw RuntimeX.mDivisionByZero(); };
    }

    @Test
    public void testFcoException() throws Exception
    {
        Continuation<FirstClassObject, RuntimeX> cont = new Continuation<>( RuntimeX.class );

        Continuation.ToStackOp<FirstClassObject> tsfco = c -> throwRtx( i1, i2, c );

        try
        {
            cont.toStack( tsfco );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.DIVISION_BY_ZERO, rx.getCode() );
        }
    }
}
