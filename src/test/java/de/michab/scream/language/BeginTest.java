/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamEvaluator;
import de.michab.scream.fcos.Cons;

public class BeginTest extends ScreamBaseTest
{
    @Test
    public void beginTest_1() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (begin
                  (define seq '(1))
                  (set! seq (append seq '(2)))
                  (set! seq (append seq '(3)))
                  seq
                )
                """ );
        assertEquals( "(1 2 3)", result.toString() );
    }

    @Test
    public void beginTest_2() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (begin)
                """ );
        assertEquals( Cons.NIL, result );
    }

    @Test
    public void _beginTest() throws Exception
    {
        var expected =
                readSingleExpression( "(1 2 3)", Cons.class );

        _contTest(
                """
                (begin
                  (define seq '(1))
                  (set! seq (append seq '(2)))
                  (set! seq (append seq '(3)))
                  seq
                )
                """,
                expected,
                expected::equal);
    }
}
