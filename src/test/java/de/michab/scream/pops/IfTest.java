/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */

package de.michab.scream.pops;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamEvaluator;

public class IfTest extends ScreamBaseTest
{
    @Test
    public void ifTest() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (if #t 313 0)
                """ );
        assertEquals( i313, result );
    }

    @Test
    public void _ifTest() throws Exception
    {
        _contTest(
                """
                (if #t 313 0)
                """,
                i313 );
    }
}
