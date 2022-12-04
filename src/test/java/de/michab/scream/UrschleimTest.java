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
import static org.junit.jupiter.api.Assertions.fail;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.jupiter.api.Test;

import de.michab.scream.frontend.SchemeParser;
import urschleim.Holder;

public class UrschleimTest extends ScreamBaseTest
{
    private static Logger LOG = Logger.getLogger( UrschleimTest.class.getName() );

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
        env.define( symbol, SchemeInteger.createObject( 313 ) );

        Holder<FirstClassObject> r =
                new Holder<FirstClassObject>( Cons.NIL );

        Holder<ScreamException> error =
                new Holder<>( null );

        Continuation.trampoline(
                symbol.evaluate( env,
                        Continuation.endCall( s -> r.set( s ) ) ),
                s -> error.set( s ));

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
        Holder<ScreamException> error =
                new Holder<>( null );

        Continuation.trampoline(
                symbol.evaluate( env,
                        Continuation.endCall( s -> r.set( s ) ) ),
                s -> error.set(s));

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
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (%syntax (xquote value) value)
                (xquote micbinz)
                """ );
        assertEquals( s( "micbinz" ), result );

        FirstClassObject opCall =
                new SchemeParser( "(xquote not-defined-yet)" ).getExpression();
        assertInstanceOf( Cons.class, opCall );

        var env = se.getInteraction();

        Holder<FirstClassObject> r =
                new Holder<FirstClassObject>( Cons.NIL );
        Holder<ScreamException> error =
                new Holder<>( null );

        Continuation.trampoline(
                opCall.evaluate( env,
                        Continuation.endCall( s -> r.set( s ) ) ),
                s -> error.set( s ) );

        if ( error.get() != null )
        {
            LOG.log( Level.SEVERE, error.get().getMessage(), error.get() );
            fail();
        }

        assertNotNull(
                r.get() );
        assertInstanceOf(
                Symbol.class,
                r.get() );
        assertEquals(
                Symbol.createObject( "not-defined-yet" ),
                r.get() );
    }

    @Test
    public void procedureTest() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (define (add2 value) (+ value 2))
                (add2 311)
                """ );
        assertEquals( i313, result );

        FirstClassObject opCall =
                new SchemeParser( "(add2 311)" ).getExpression();
        assertInstanceOf( Cons.class, opCall );

        var env = se.getInteraction();

        Holder<FirstClassObject> r =
                new Holder<FirstClassObject>( Cons.NIL );
        Holder<ScreamException> error =
                new Holder<>( null );

        Continuation.trampoline(
                opCall.evaluate( env,
                        Continuation.endCall( s -> r.set( s ) ) ),
                s -> error.set( s ) );

        if ( error.get() != null )
        {
            LOG.log( Level.SEVERE, error.get().getMessage(), error.get() );
            fail();
        }

        assertNotNull(
                r.get() );
        assertInstanceOf(
                SchemeInteger.class,
                r.get() );
        assertEquals(
                ScreamBaseTest.i313,
                r.get() );
    }

    @Test
    public void syntaxQuoteTest() throws Exception
    {
        _contTest(
                "'lumumba'",
                s("lumumba") );
    }

    @Test
    public void syntaxAssignmentTest() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var env = se.getInteraction();
        env.define( s313, i1  );

        FirstClassObject opCall =
                new SchemeParser( "(set! threethirteen 313)" ).getExpression();
        assertInstanceOf( Cons.class, opCall );

        Holder<FirstClassObject> r =
                new Holder<FirstClassObject>( Cons.NIL );
        Holder<ScreamException> error =
                new Holder<>( null );

        Continuation.trampoline(
                opCall.evaluate( env,
                        Continuation.endCall( s -> r.set( s ) ) ),
                s -> error.set( s ) );

        if ( error.get() != null )
        {
            LOG.log( Level.SEVERE, error.get().getMessage(), error.get() );
            fail();
        }

        assertEquals(
                Cons.NIL,
                r.get() );
        assertEquals(
                ScreamBaseTest.i313,
                env.get( ScreamBaseTest.s313 ) );
    }

}
