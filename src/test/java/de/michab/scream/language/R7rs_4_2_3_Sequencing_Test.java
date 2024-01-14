/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream.language;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Cons;

public class R7rs_4_2_3_Sequencing_Test extends ScreamBaseTest
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
    public void beginTest_3() throws Exception
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
    public void chez_beginTest_4() throws Exception
    {
        expectFco(
                """
                (begin
                  (define donald 313))
                donald
                """,
                i313 );
    }
}
