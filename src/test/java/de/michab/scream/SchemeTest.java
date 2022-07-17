package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

public class SchemeTest
{
    @Test
    public void addTest() throws Exception
    {
        SchemeInterpreter2 si = new SchemeInterpreter2();
        var se = si.getScriptEngine();

        var result = se.eval( "(+ 3 310)" );

        assertEquals( "313", result.toString() );
    }

    @Test
    public void subtractTest() throws Exception
    {
        SchemeInterpreter2 si = new SchemeInterpreter2();
        var se = si.getScriptEngine();

        var result = se.eval( "(- 320 7)" );

        assertEquals( "313", result.toString() );
    }
}
