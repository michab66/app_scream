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
import de.michab.scream.ScreamEvaluator;

public class MathTest extends ScreamBaseTest
{
    @Test
    public void mathEquals() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (= 313 313)
                """ );
        assertEquals( SchemeBoolean.T, result );
    }
}
