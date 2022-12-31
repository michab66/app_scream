/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.pops;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import org.junit.jupiter.api.Test;

import de.michab.scream.Cons;
import de.michab.scream.Continuation;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.Operation;
import de.michab.scream.Scream;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamEvaluator;
import de.michab.scream.frontend.SchemeParser;

public class SyntaxBeginTest extends ScreamBaseTest
{
    @Test
    public void exists() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                begin
                """ );
        assertInstanceOf( Operation.class, result );
    }

    @Test
    void _begin() throws Exception
    {
        Scream si = new Scream();
        ScreamEvaluator se = (ScreamEvaluator)si.getScriptEngine();

        Environment env =
                se.getInteraction();

        Cons cons = (Cons)new SchemeParser(
            """
                ((define one 1)
                 (define two 2)
                 313)
            """ ).getExpression();

        FirstClassObject r = Continuation.toStack(
                env,
                (e,c) -> Primitives._x_begin(
                        e,
                        cons,
                        c ) );

        assertEqualq( i313, r );
        assertEqualq( i1, env.get( s1 ) );
        assertEqualq( i2, env.get( s2 ) );
    }
}
