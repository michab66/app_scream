/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamEvaluator;
import de.michab.scream.ScreamException.Code;
import de.michab.scream.fcos.SchemeBoolean;

public class MathTest extends ScreamBaseTest
{
    @Test
    public void mathEquals_t() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (= 313 313)
                """ );
        assertEquals( SchemeBoolean.T, result );
    }

    @Test
    public void mathEquals_f() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (= 121 313)
                """ );
        assertEquals( SchemeBoolean.F, result );
    }

    @Test
    public void _equalsCompile() throws Exception
    {
        expectFco( "(= 121 121)", SchemeBoolean.T );
    }

    @Test
    public void _equalsCompile2() throws Exception
    {
        expectFco( "(define i 121)(= i 121)", SchemeBoolean.T );
    }

    @Test
    @Disabled( "https://github.com/urschleim/scream/issues/157" )
    public void addition_err_1() throws Exception
    {
        expectError( "(+ 1 'i)", Code.TYPE_ERROR );
    }
}
