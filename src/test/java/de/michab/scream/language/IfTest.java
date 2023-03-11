/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.SchemeBoolean;

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
        expectFco(
                """
                (if #t 313 0)
                """,
                i313 );
    }
}
