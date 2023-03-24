/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.fcos;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.smack.util.Holder;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.util.Continuation;
import de.michab.scream.util.Continuation.Cont;
import de.michab.scream.util.Continuation.Thunk;

public class ContinuationTest extends ScreamBaseTest
{
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
                null,
                Exception.class );

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
    @Disabled
    @Test
    public void testException2() throws Exception
    {
        Continuation<Integer, ArithmeticException> continuation =
                new Continuation<>( ArithmeticException.class );

        Holder<ArithmeticException> aeh =
                new Holder<>();

        Cont<ArithmeticException> handler =
                ae -> {
                    aeh.set( ae );
                    return null;
                };

        int result = continuation.toStack(
                cont-> addx(3,4,cont),
                handler,
                ArithmeticException.class );

        assertEquals( 7, result );
    }
}
