/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 2023 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Procedure;
import de.michab.scream.fcos.SchemeBoolean;

/**
 * r7rs 6.13.1 Ports
 *
 * @author MICBINZ
 */
public class R7rs_6_13_1_Ports_Test extends ScreamBaseTest
{
    /**
     * p56
     */
    @Test
    public void portPredicates_1() throws Exception
    {
        var se = scriptEngine();

        {
            var result = se.evalFco(
                """
                (port? 1)
                """ );
            assertEquals( SchemeBoolean.F, result );
        }
        {
            var result = se.evalFco(
                """
                (port? '(a))
                """ );
            assertEquals( SchemeBoolean.F, result );
        }
        {
            var result = se.evalFco(
                """
                (port? 'a)
                """ );
            assertEquals( SchemeBoolean.F, result );
        }
        {
            var result = se.evalFco(
                """
                (port? #(1 2 3))
                """ );
            assertEquals( SchemeBoolean.F, result );
        }
    }

    /**
     * p56
     */
    @Test
    public void basic_1() throws Exception
    {
        var se = scriptEngine();

        {
            var result = se.evalFco(
                """
                current-input-port
                """ );
            assertInstanceOf( Procedure.class, result );
        }
        {
            var result = se.evalFco(
                """
                (port? (current-input-port))
                """ );
            assertEquals( SchemeBoolean.T, result );
        }
    }

}
