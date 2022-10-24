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

        var result = se.evalFco( "(+ 3 310)" );

        assertEquals( i313, result );
    }

    @Test
    public void subtractTest() throws Exception
    {
        var se = scriptEngine();

        var result = se.evalFco( "(- 320 7)" );

        assertEquals( i313, result );
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

        SchemeEvaluator2 se = scriptEngine();

        se.evalFco( String.format( "(load \"%s\")", tf.toString() ) );

        var one = se.evalFco( "one" );
        var two = se.evalFco( "two" );

        assertEquals( i1, one );
        assertEquals( i2, two );
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
        SchemeEvaluator2 se1 = (SchemeEvaluator2)si.getScriptEngine();
        SchemeEvaluator2 se2 = (SchemeEvaluator2)si.getScriptEngine();

        var result = se1.evalFco( "(+ 1 1)" );
        assertEquals( i2, result );
        result = se1.evalFco(
                """
                (define (+ a b)
                (- a b))
                """ );
        result = se1.evalFco( "(+ 1 1)" );
        assertEquals( i(0), result );

        result = se2.evalFco( "(+ 1 1)" );
        assertEquals( i2, result );
    }

    @Test
    public void partitionTest2() throws Exception
    {
        SchemeInterpreter2 si = new SchemeInterpreter2();
        SchemeEvaluator2 se1 = (SchemeEvaluator2)si.getScriptEngine();
        SchemeEvaluator2 se2 = (SchemeEvaluator2)si.getScriptEngine();

        var result = se1.evalFco( "(+ 1 1)" );
        assertEquals( i2, result );
        result = se1.evalFco(
                """
                (set! + (lambda (a b) 313))
                """ );
        result = se1.evalFco( "(+ 1 1)" );
        assertEquals( i313, result );

        result = se2.evalFco( "(+ 1 1)" );
        assertEquals( i2, result );
    }
}
