/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.pops;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.smack.util.Holder;

import de.michab.scream.Cons;
import de.michab.scream.Continuation;
import de.michab.scream.FirstClassObject;
import de.michab.scream.Operation;
import de.michab.scream.SchemeInteger;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamEvaluator;
import de.michab.scream.ScreamException;

public class SyntaxTimeTest extends ScreamBaseTest
{
    @Test
    public void exists() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                %time
                """ );
        assertInstanceOf( Operation.class, result );
    }

    private void validateResult( FirstClassObject result )
    {
        Cons cons = (Cons)result;
        assertFalse( ((Cons)result).isProperList() );
        SchemeInteger car = (SchemeInteger)cons.getCar();
        SchemeInteger cdr = (SchemeInteger)cons.getCdr();

        assertTrue( 3 == cdr.asLong() );
        assertTrue( car.asLong() >= 0 );
    }

    @Test
    void time_1() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (%time (+ 1 2))
                """ );

        validateResult( result );
    }

    @Test
    void time_2() throws Exception
    {
        Cons cons = (Cons)parse(
            """
                (%time (+ 1 2))
            """ );

        Holder<FirstClassObject> r =
                new Holder<>( null );
        Holder<ScreamException> error =
                new Holder<>( null );

        Continuation.trampoline(
                cons.evaluate(
                        scriptEngine().getInteraction(),
                        Continuation.endCall( s -> r.set( s ) ) ),
                s -> error.set( s ) );

        if ( error.get() != null )
            throw error.get();

        validateResult( r.get() );
    }

}
