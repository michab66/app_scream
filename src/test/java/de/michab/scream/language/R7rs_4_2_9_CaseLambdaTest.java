/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamEvaluator;
import de.michab.scream.fcos.Cons;

public class R7rs_4_2_9_CaseLambdaTest extends ScreamBaseTest
{
    @Test
    public void exists() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        {
        var result = se.evalFco(
                """
    (define range
      (case-lambda
        (() #f)
        ((e) (+ 1 e))
        ((e f) (+ f e))
        ((e f g) (+ g f e))))
                """ );

        assertEqualq( Cons.NIL, result );
        }
        {
            var result = se.evalFco( "(range)" );
            assertEqualq( bFalse, result );
        }
        {
            var result = se.evalFco( "(range 1)" );
            assertEqualq( i2, result );
        }
        {
            var result = se.evalFco( "(range 1 2)" );
            assertEqualq( i3, result );
        }
        {
            var result = se.evalFco( "(range 1 2 3)" );
            assertEqualq( i(6), result );
        }
        try
        {
            se.evalFco( "(range 1 2 3 4)" );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.WRONG_NUMBER_OF_ARGUMENTS, rx.getCode() );
            assertEquals( "4", rx.getArgument( 0 ) );
        }
    }
}
