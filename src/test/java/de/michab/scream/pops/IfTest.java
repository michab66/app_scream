/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */

package de.michab.scream.pops;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import de.michab.scream.SchemeEvaluator2;
import de.michab.scream.SchemeInterpreter2;
import de.michab.scream.ScreamBaseTest;

public class IfTest extends ScreamBaseTest
{
    @Test
    public void ifTest() throws Exception
    {
        SchemeEvaluator2 se = (SchemeEvaluator2)new SchemeInterpreter2().getScriptEngine();

        var result = se.eval(
                """
                (if #t 313 0)
                """ );
        assertEquals( result, ScreamBaseTest.i313 );
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
