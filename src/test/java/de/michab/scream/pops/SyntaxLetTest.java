/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.pops;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamEvaluator;
import de.michab.scream.fcos.Operation;

public class SyntaxLetTest extends ScreamBaseTest
{
    @Test
    public void letExists() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                let
                """ );
        assertInstanceOf( Operation.class, result );
    }
    @Test
    public void letAsteriscExists() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                let*
                """ );
        assertInstanceOf( Operation.class, result );
    }
    @Test
    public void letrecExists() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                letrec
                """ );
        assertInstanceOf( Operation.class, result );
    }
}
