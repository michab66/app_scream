package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

public class SyntaxTest
{
    Symbol xx( String s )
    {
        return null;
    }

    @Test
    public void toJava() throws Exception
    {
        var se = new SchemeInterpreter2().getScriptEngine();

        var result = se.eval(
                """
                (%syntax (xquote value) value)
                (xquote micbinz)
                """ );
        assertEquals( result, Symbol.createObject( "micbinz" ) );
    }
}
