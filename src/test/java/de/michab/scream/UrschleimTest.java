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
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.jupiter.api.Test;

import de.michab.scream.frontend.SchemeParser;
import urschleim.Continuation;
import urschleim.Holder;

public class UrschleimTest
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
        env.set( symbol, SchemeInteger.createObject( 313 ) );

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
        SchemeEvaluator2 se = (SchemeEvaluator2)new SchemeInterpreter2().getScriptEngine();

        var result = se.eval(
                """
                (%syntax (xquote value) value)
                (xquote micbinz)
                """ );
        assertEquals( result, Symbol.createObject( "micbinz" ) );

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
        SchemeEvaluator2 se = (SchemeEvaluator2)new SchemeInterpreter2().getScriptEngine();

        var result = se.eval(
                """
                (define (add2 value) (+ value 2))
                (add2 311)
                """ );
        assertEquals( result, TestUtil.i313 );

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
                TestUtil.i313,
                r.get() );
    }

    @Test
    public void syntaxIfTest() throws Exception
    {
        SchemeEvaluator2 se = (SchemeEvaluator2)new SchemeInterpreter2().getScriptEngine();

        var result = se.eval(
                """
                (if #t 313 0)
                """ );
        assertEquals( result, TestUtil.i313 );

        FirstClassObject opCall =
                new SchemeParser( "(if #t 313 0)" ).getExpression();
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
                TestUtil.i313,
                r.get() );
    }

    @Test
    public void syntaxAssignmentTest() throws Exception
    {
        SchemeEvaluator2 se = (SchemeEvaluator2)new SchemeInterpreter2().getScriptEngine();

        var result = se.eval(
                """
                (define x 0)
                (set! x 313)
                x
                """ );
        assertEquals( result, TestUtil.i313 );

        var env = se.getInteraction();
        env.set( TestUtil.s313, TestUtil.i1  );

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
                TestUtil.i313,
                env.get( TestUtil.s313 ) );
    }

    @Test
    public void listEvalTest() throws Exception
    {
        Environment env = new Environment();
        env.set(
                TestUtil.s1,
                TestUtil.i1 );
        env.set(
                TestUtil.s2,
                TestUtil.i2 );
        env.set(
                TestUtil.s3,
                TestUtil.i3 );
        env.set(
                TestUtil.s4,
                TestUtil.i4 );

        FirstClassObject list1234c =
                new SchemeParser( "(one two three four)" ).getExpression();
        assertInstanceOf( Cons.class, list1234c );
        var array1234 = ((Cons)list1234c).asArray();

        Holder<FirstClassObject[]> r =
                new Holder<>( null );
        Holder<ScreamException> error =
                new Holder<>( null );

        Continuation.trampoline(
                Continuation.listEval(
                        env,
                        array1234,
                        Continuation.endCall( s -> r.set( s ) ) ),
                s -> error.set( s ) );

        assertNotNull(
                r.get() );
        assertEquals(
                TestUtil.i1,
                r.get()[0] );
        assertEquals(
                TestUtil.i2,
                r.get()[1] );
        assertEquals(
                TestUtil.i3,
                r.get()[2] );
        assertEquals(
                TestUtil.i4,
                r.get()[3] );
        assertNull(
                error.get() );
    }

    @Test
    void _begin() throws Exception
    {
        SchemeInterpreter2 si = new SchemeInterpreter2();
        SchemeEvaluator2 se = (SchemeEvaluator2)si.getScriptEngine();

        Environment env =
                se.getInteraction();

        Cons cons = (Cons)new SchemeParser(
            """
                ((define one 1)
                 (define two 2)
                 313)
            """ ).getExpression();

        Holder<FirstClassObject> r =
                new Holder<>( null );
        Holder<ScreamException> error =
                new Holder<>( null );

        Continuation.trampoline(
                Continuation._begin(
                        env,
                        cons,
                        Continuation.endCall( s -> r.set( s ) ) ),
                s -> error.set( s ) );

        assertTrue( TestUtil.i313.equal( r.get() ) );
        assertTrue( TestUtil.i1.equal( env.get( TestUtil.s1 ) ) );
        assertTrue( TestUtil.i2.equal( env.get( TestUtil.s2 ) ) );
    }
}
