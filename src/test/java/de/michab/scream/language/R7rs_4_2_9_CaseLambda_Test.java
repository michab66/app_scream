/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;

public class R7rs_4_2_9_CaseLambda_Test extends ScreamBaseTest
{
    @Test
    public void exists() throws Exception
    {
        var t = makeTester();

        t.execute(
                """
                (define range
                  (case-lambda
                    (() #f)
                    ((e) (+ 1 e))
                    ((e f) (+ f e))
                    ((e f g) (+ g f e))))
                """ );

            t.expectFco(
                    "(range)",
                    bFalse );
            t.expectFco(
                    "(range 1)",
                    i2 );
            t.expectFco(
                    "(range 1 2)",
                    i3 );
            t.expectFco(
                    "(range 1 2 3)",
                    i(6) );
            var rx = t.expectError(
                    "(range 1 2 3 4)",
                    Code.WRONG_NUMBER_OF_ARGUMENTS );
            assertEquals( i4, rx.getArgument( 0 ) );
    }

    @Test
    public void caseLambda() throws Exception
    {
        var t = makeTester();

        t.execute(
                """
                (define default3
                  (case-lambda
                    (()
                      (default3 'ad 'bd 'cd))
                    ((a)
                      (default3 a 'bd 'cd))
                    ((a b)
                      (default3 a b 'cd))
                    ((a b c)
                      (list a b c))))
                """ );

        t.expectFco(
                    "(default3)",
                    parse( "(ad bd cd)" ) );
        t.expectFco(
                    "(default3 1)",
                    parse( "(1 bd cd)" ) );
        t.expectFco(
                    "(default3 1 2)",
                    parse( "(1 2 cd)" ) );
        t.expectFco(
                    "(default3 1 2 3)",
                    parse( "(1 2 3)" ) );

        var rx = t.expectError(
                "(default3 1 2 3 4)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
        assertEquals( i4, rx.getArgument( 0 ) );
    }
}
