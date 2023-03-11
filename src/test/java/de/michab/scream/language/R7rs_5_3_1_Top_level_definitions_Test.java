/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamException.Code;

/**
 * r7rs 5.3.1 Top level definitions
 *
 * @author MICBINZ
 */
public class R7rs_5_3_1_Top_level_definitions_Test extends ScreamBaseTest
{
    /**
     * p26
     */
    @Test
    public void top_level_definitions_1() throws Exception
    {
        var result = scriptEngine().evalFco(
"""
(define add3
  (lambda (x) (+ x 3)))
(add3 3)
""" );
        assertEqualq(
                i(6),
                result );
    }

    /**
     * p26
     */
    @Test
    public void top_level_definitions_2() throws Exception
    {
        var result = scriptEngine().evalFco(
"""
                (define first car)
                (first '(1 2))
""" );
        assertEqualq(
                i1,
                result );
    }

    /**
     * p26
     */
    @Test
    public void top_level_definitions_x() throws Exception
    {
        try
        {
            scriptEngine().evalFco(
                    """
                    (set! unbound-variable 0)
                    """
            );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.SYMBOL_NOT_ASSIGNABLE, rx.getCode() );
            assertEquals( "unbound-variable", rx.getArgument(0).toString() );
        }
    }

}
