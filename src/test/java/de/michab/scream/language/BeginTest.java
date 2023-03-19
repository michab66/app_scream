/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.language;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Cons;

public class BeginTest extends ScreamBaseTest
{
    @Test
    public void beginTest_1() throws Exception
    {
        expectFco(
                """
                (begin
                  (define seq '(1))
                  (set! seq (append seq '(2)))
                  (set! seq (append seq '(3)))
                  seq
                )
                """,
                "(1 2 3)" );
    }

    @Test
    public void beginTest_2() throws Exception
    {
        expectFco(
                """
                (begin)
                """,
                Cons.NIL );
    }

    @Test
    public void _beginTest() throws Exception
    {
        expectFco(
                """
                (begin
                  (define seq '(1))
                  (set! seq (append seq '(2)))
                  (set! seq (append seq '(3)))
                  seq
                )
                """,
                "(1 2 3)" );
    }
}
