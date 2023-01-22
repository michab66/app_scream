/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.smack.util.Holder;

import de.michab.scream.Continuation.Cont;
import de.michab.scream.Continuation.Cont2;
import de.michab.scream.Continuation.Thunk;

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
        Continuation.thunkCount(0);

        int result = Continuation.toStack(
                cont -> add(3,4,cont) );

        assertEquals( 1, Continuation.thunkCount() );
        assertEquals( 7, result );
    }

    @Test
    public void basic2() throws Exception
    {
        Continuation.thunkCount(0);

        int result = Continuation.toStack(
                cont-> addd(3,4,cont) );

        assertEquals( 2, Continuation.thunkCount() );
        assertEquals( 7, result );
    }

    @Test
    public void testSuccess() throws Exception
    {
        int result = Continuation.toStack(
                cont-> add(3,4,cont),
                null,
                Exception.class );

        assertEquals( 7, result );
    }

    @Test
    public void testException()
    {
        try
        {
            @SuppressWarnings("unused")
            int result = Continuation.toStack(
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
        Holder<ArithmeticException> aeh =
                new Holder<>();

        Cont2<ArithmeticException> handler =
                ae -> {
                    aeh.set( ae );
                    return null;
                };

        int result = Continuation.toStack(
                cont-> addx(3,4,cont),
                handler,
                ArithmeticException.class );

        assertEquals( 7, result );
    }


}
