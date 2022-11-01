package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import urschleim.Continuation;
import urschleim.Holder;

public class SyntaxTest extends ScreamBaseTest
{
    private static Logger LOG = Logger.getLogger( SyntaxTest.class.getName() );

    @Test
    public void syntaxCaseTest() throws Exception
    {
        var env = scriptEngine().getInteraction();

        Cons opCall = readSingleExpression(
                """
                (case (* 2 3)
                 ((2 3 5 7) 'prime)
                 ((1 4 6 8 s) 'composite))
                """,
                Cons.class );

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
                s("composite"),
                r.get() );
    }

    @Test
    public void syntaxCaseTest_2() throws Exception
    {
        var env = scriptEngine().getInteraction();

        Cons opCall = readSingleExpression(
                """
                (case (+ 3 4)
                 ((2 3 5 7) 'prime)
                 ((1 4 6 8) 'composite))
                """,
                Cons.class );

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
                s("prime"),
                r.get() );
    }

    /**
     * Cons error: Duplicate symbol.
     */
    @Test
    @Disabled( "not implemented" )
    public void syntaxCaseError_1() throws Exception
    {
        var env = scriptEngine().getInteraction();

        Cons opCall = readSingleExpression(
                """
                (case (+ 3 4)
                 ((2 3 5 7 7) 'prime)
                 ((1 4 6 8) 'composite))
                """,
                Cons.class );

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
                s("prime"),
                r.get() );
    }

    @Test
    public void syntaxLambdaTest() throws Exception
    {
        var env = scriptEngine().getInteraction();

        Cons opCall = readSingleExpression(
                "((lambda (x y) (+ x y)) 1 2)",
                Cons.class );

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
                i3,
                r.get() );
    }

    @Test
    public void syntaxSyntaxTest() throws Exception
    {
        var se = scriptEngine();

        var result = se.eval(
                """
                (%syntax (xquote value) value)
                (xquote micbinz)
                """ );
        assertEquals( "micbinz", result );
    }
}
