/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import de.michab.scream.SchemeBoolean;
import de.michab.scream.ScreamBaseTest;

public class IfTest extends ScreamBaseTest
{
    @Test
    public void ifTest() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (if #t 313 0)
                """ );
        assertEquals( i313, result );
    }

    @Test
    public void ifTestNoElse() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (if #f 313)
                """ );
        assertEquals( SchemeBoolean.F, result );
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
