/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.pops;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamEvaluator;
import de.michab.scream.fcos.Operation;

public class SyntaxSyntaxTest extends ScreamBaseTest
{
    @Test
    public void exists() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                %syntax
                """ );
        assertInstanceOf( Operation.class, result );
    }


    @Test
    public void syntaxSyntaxTest() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (%syntax (xquote value) value)
                (xquote micbinz)
                """ );
        assertEquals( s( "micbinz" ), result );
    }

}
