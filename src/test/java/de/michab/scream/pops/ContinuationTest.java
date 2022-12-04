/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.pops;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamException;
import de.michab.scream.pops.Primitives.Cont;
import de.michab.scream.pops.Primitives.Thunk;
import urschleim.Holder;

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

    @Test
    void _basic() throws Exception
    {
        Primitives.thunkCount(0);
        Holder<Integer> result =
                new Holder<>( null );
        Holder<ScreamException> error =
                new Holder<>( null );

        Primitives.trampoline(
                add( 3,
                     4,
                     Primitives.endCall( s -> result.set( s ) ) ),
                e -> error.set( e ) );

        assertEquals( 1, Primitives.thunkCount() );
        assertEquals( 7, result.get() );
    }

    @Test
    void _basic2() throws Exception
    {
        Primitives.thunkCount(0);
        Holder<Integer> result =
                new Holder<>( null );
        Holder<ScreamException> error =
                new Holder<>( null );

        Primitives.trampoline(
                addd( 3,
                     4,
                     Primitives.endCall( s -> result.set( s ) ) ),
                e -> error.set( e ) );

        assertEquals( 2, Primitives.thunkCount() );
        assertEquals( 7, result.get() );
    }
}
