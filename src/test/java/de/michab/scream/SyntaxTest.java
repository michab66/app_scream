package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

public class SyntaxTest extends ScreamBaseTest
{
    @Test
    public void toJava() throws Exception
    {
        var se = scriptEngine();

        var result = se.eval(
                """
                (%syntax (xquote value) value)
                (xquote micbinz)
                """ );
        assertEquals( "micbinz", result );
    }
}
