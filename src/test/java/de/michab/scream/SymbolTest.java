/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import de.michab.scream.fcos.Symbol;

public class SymbolTest extends ScreamBaseTest
{
    @Test
    public void constantness() throws Exception
    {
        assertTrue( s1.isConstant() );
    }

    @Test
    public void toJava() throws Exception
    {
        ScreamBaseTest.toJava_(
                Symbol.class,
                String.class,
                "micbinz",
                Symbol::createObject );
    }

    @Test
    public void arrowSymbolTest() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                '=>
                """ );
        assertEquals( s("=>"), result );
    }

}
