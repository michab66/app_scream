/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2023 Michael G. Binz
 */
package de.michab.scream.language;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;

/**
 * r7rs 4.2.4 Iteration.
 *
 * @author MICBINZ
 */
public class R7rs_4_2_5_Delayed_evaluation_Test extends ScreamBaseTest
{
    /**
     * p19
     */
    @Test
    public void promise_q() throws Exception
    {
        expectFco(
            """
            (promise? (delay (+ 10 3)))
            """,
            bTrue );
    }

    /**
     * p18
     */
    @Test
    public void force_1() throws Exception
    {
        expectFco(
            """
            (force (delay (+ 1 2)))
            """,
            i3 );
    }

    /**
     * p18
     */
    @Test
    public void force_2() throws Exception
    {
        expectFco(
            """
            (let ((p (delay (+ 1 2))))
              (list(force p)(force p)))
            """,
            parse("(3 3)") );
    }

    /**
     * p19
     */
    @Test
    public void make_promise() throws Exception
    {
        expectFco(
            """
            (force (make-promise (+ 10 3)))

            """,
            i(13) );
    }

}
