package de.michab.scream.pops;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import org.junit.jupiter.api.Test;

import de.michab.scream.Operation;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamEvaluator;

public class SyntaxTimeTest extends ScreamBaseTest
{
    @Test
    public void syntaxTime_basic() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                %time
                """ );
        assertInstanceOf( Operation.class, result );
    }
}
