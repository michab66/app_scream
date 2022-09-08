package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.File;
import java.io.FileWriter;
import java.nio.file.Files;

import javax.script.ScriptException;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamException.Code;

public class SchemeTest
{
    @Test
    public void errorTest() throws Exception
    {
        SchemeInterpreter2 si = new SchemeInterpreter2();
        var se = si.getScriptEngine();

        try
        {
            se.eval( "(error 3 310)" );
            fail();
        }
        catch ( ScriptException e )
        {
            assertInstanceOf( ScreamException.class, e.getCause() );
            ScreamException sex = (ScreamException)e.getCause();
            // TYPE_ERROR
            assertEquals( Code.TYPE_ERROR, sex.getCode() );
            assertEquals( 11, sex.getId() );
        }
    }

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

    @Test
    public void loadTest() throws Exception
    {
        var tp = Files.createTempFile( getClass().getSimpleName(), ".tmp" );
        File tf = tp.toFile();
        tf.deleteOnExit();
        try ( var fw = new FileWriter( tf ) )
        {
            fw.write(
                    """
(define one 1)
(define two 2)
                    """);
        }

        SchemeInterpreter2 si = new SchemeInterpreter2();
        var se = si.getScriptEngine();

        se.eval( String.format( "(load \"%s\")", tf.toString() ) );

        var one = se.eval( "one" );
        var two = se.eval( "two" );

        assertEquals( "1", one.toString() );
        assertEquals( "2", two.toString() );
    }
}
