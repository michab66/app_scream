package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.jupiter.api.Test;

import urschleim.Continuation;
import urschleim.Holder;

public class SyntaxTest extends ScreamBaseTest
{
    private static Logger LOG = Logger.getLogger( SyntaxTest.class.getName() );

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
}
