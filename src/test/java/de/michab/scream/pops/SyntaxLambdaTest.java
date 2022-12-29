/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.pops;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.jupiter.api.Test;
import org.smack.util.Holder;

import de.michab.scream.Cons;
import de.michab.scream.Continuation;
import de.michab.scream.FirstClassObject;
import de.michab.scream.Operation;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamEvaluator;
import de.michab.scream.ScreamException;

public class SyntaxLambdaTest extends ScreamBaseTest
{
    private static Logger LOG = Logger.getLogger( SyntaxLambdaTest.class.getName() );

    @Test
    public void exists() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                lambda
                """ );
        assertInstanceOf( Operation.class, result );
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
