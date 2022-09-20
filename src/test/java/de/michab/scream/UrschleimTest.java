/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 2022 Michael G. Binz
 */
package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

import de.michab.scream.frontend.SchemeParser;
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

    @Test
    public void operationTest() throws Exception
    {
        FirstClassObject add313 =
                new SchemeParser( "(+ 300 13)" ).getExpression();
        assertInstanceOf( Cons.class, add313 );

        SchemeInterpreter2 si = new SchemeInterpreter2();
        var se = (SchemeEvaluator2)si.getScriptEngine();

        var env = se.getInteraction();

        Holder<FirstClassObject> r =
                new Holder<FirstClassObject>( Cons.NIL );
        Holder<RuntimeX> error =
                new Holder<RuntimeX>( null );
        Continuation c =
                new Continuation( s -> error.set( s ) );

        c.trampoline(
                add313.evaluate( env,
                        Continuation.endCall( s -> r.set( s ) ) ));

        assertNotNull(
                r.get() );
        assertInstanceOf(
                SchemeInteger.class,
                r.get() );
        assertEquals(
                313,
                ((SchemeInteger)r.get()).asLong() );
        assertNull(
                error.get() );
    }
}
