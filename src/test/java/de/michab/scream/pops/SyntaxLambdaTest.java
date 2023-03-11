/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.pops;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import org.junit.jupiter.api.Test;

import de.michab.scream.Scream;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamEvaluator;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Operation;

public class SyntaxLambdaTest extends ScreamBaseTest
{
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

        FirstClassObject r = Scream.toStack(
                c -> opCall.evaluate( env, c ) );

        assertEquals(
                i3,
                r );
    }

}
