/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 2022 Michael G. Binz
 */
package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

import urschleim.Continuation;
import urschleim.Holder;

public class UrschleimTest
{
    @Test
    public void typeIntegerTest() throws Exception
    {
        Holder<RuntimeX> error =
                new Holder<RuntimeX>( null );

        var i = SchemeInteger.createObject( 313 );

        Holder<FirstClassObject> r =
                new Holder<FirstClassObject>( Cons.NIL );

        Continuation c = new Continuation( s -> error.set( s ) );

        c.trampoline(
                i.evaluate( null,
                        Continuation.endCall( s -> r.set( s ) ) ));

        assertEquals( "313", r.get().toString() );
        assertNull( error.get() );
    }

    @Test
    public void typeSymbolTest() throws Exception
    {
        var symbol = Symbol.createObject( "car" );
        var env = new Environment();
        env.set( symbol, SchemeInteger.createObject( 313 ) );

        Holder<FirstClassObject> r =
                new Holder<FirstClassObject>( Cons.NIL );

        Holder<RuntimeX> error =
                new Holder<RuntimeX>( null );
        Continuation c = new Continuation( s -> error.set( s ) );

        c.trampoline(
                symbol.evaluate( env,
                        Continuation.endCall( s -> r.set( s ) ) ));

        assertEquals( "313", r.get().toString() );
        assertNull( error.get() );
    }

    @Test
    public void typeSymbolErrorTest() throws Exception
    {
        var symbol = Symbol.createObject( "car" );
        var env = new Environment();

        Holder<FirstClassObject> r =
                new Holder<FirstClassObject>( Cons.NIL );
        Holder<RuntimeX> error =
                new Holder<RuntimeX>( null );
        Continuation c =
                new Continuation( s -> error.set( s ) );

        c.trampoline(
                symbol.evaluate( env,
                        Continuation.endCall( s -> r.set( s ) ) ));

        assertNull(
                r.get() );
        assertNotNull(
                error.get() );
        assertEquals(
                ScreamException.Code.SYMBOL_NOT_DEFINED,
                error.get().getCode() );
    }
}
