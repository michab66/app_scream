package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.File;
import java.io.FileWriter;
import java.nio.file.Files;
import java.util.UUID;

import javax.script.ScriptException;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamException.Code;

public class SchemeTest extends ScreamBaseTest
{
    @Test
    public void errorTest() throws Exception
    {
        var se = scriptEngine();

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
        var se = scriptEngine();

        var result = se.eval( "(+ 3 310)" );

        assertEquals( "313", result.toString() );
    }

    @Test
    public void subtractTest() throws Exception
    {
        var se = scriptEngine();

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

    @Test
    public void loadNonExistingTest() throws Exception
    {
        var se = scriptEngine();

        var nonExistingName = UUID.randomUUID().toString();
        try
        {
            se.eval( String.format( "(load \"%s\")", nonExistingName ) );
            fail();
        }
        catch ( Exception e )
        {
            assertTrue( e.getMessage().contains( nonExistingName ) );
        }
    }

    /**
     * Redefine an operation using (define ...) in an engine and
     * ensure that this does not propagate to a second engine.
     */
    @Test
    public void partitionTest() throws Exception
    {
        SchemeInterpreter2 si = new SchemeInterpreter2();
        var se1 = si.getScriptEngine();
        var se2 = si.getScriptEngine();

        var result = se1.eval( "(+ 1 1)" );
        assertEquals( "2", result.toString() );
        result = se1.eval(
                """
                (define (+ a b)
                (- a b))
                """ );
        result = se1.eval( "(+ 1 1)" );
        assertEquals( "0", result.toString() );

        result = se2.eval( "(+ 1 1)" );
        assertEquals( "2", result.toString() );
    }

    @Test
    public void partitionTest2() throws Exception
    {
        SchemeInterpreter2 si = new SchemeInterpreter2();
        var se1 = si.getScriptEngine();
        var se2 = si.getScriptEngine();

        var result = se1.eval( "(+ 1 1)" );
        assertEquals( "2", result.toString() );
        result = se1.eval(
                """
                (set! + (lambda (a b) 313))
                """ );
        result = se1.eval( "(+ 1 1)" );
        assertEquals( "313", result.toString() );

        result = se2.eval( "(+ 1 1)" );
        assertEquals( "2", result.toString() );
    }
}
