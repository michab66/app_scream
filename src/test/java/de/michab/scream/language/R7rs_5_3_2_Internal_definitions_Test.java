/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.language;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;

/**
 * r7rs 5.3.2 Internal definitions
 *
 * @author MICBINZ
 */
public class R7rs_5_3_2_Internal_definitions_Test extends ScreamBaseTest
{
    /**
     * p26
     */
    @Test
    public void internal_definitions_1() throws Exception
    {
        var result = scriptEngine().evalFco(
"""
(let ((x 5))
 (define foo (lambda (y) (bar x y)))
 (define bar (lambda (a b) (+ (* a b) a)))
 (foo (+ x 3)))
""" );
        assertEqualq(
                i(45),
                result );
    }

}
