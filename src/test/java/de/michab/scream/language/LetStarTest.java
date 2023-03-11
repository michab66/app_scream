/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;

/**
 * r7rs 4.2.2 Binding constructs, p16
 */
public class LetStarTest extends ScreamBaseTest
{
    @Test
    public void letStar_1() throws Exception
    {
        // x is defined in the second initialization
        // in difference to plain let.
        var result = scriptEngine().evalFco(
            """
            (let*
            ((x 2) (y x))
            y)
            """ );
        assertEquals( i2, result );
    }

    @Test
    public void letStar_2() throws Exception
    {
        // r7rs: The <variable>s need not be distinct.
        var result = scriptEngine().evalFco(
            """
            (let*
            ((x 2) (x 312) (x (+ 1 x)) (y x))
            y)
            """ );
        assertEquals( i313, result );
    }

}
