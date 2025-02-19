/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.File;
import java.io.FileWriter;
import java.nio.file.Files;
import java.util.UUID;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX.Code;

public class SchemeTest extends ScreamBaseTest
{
    @Test
    public void errorTest() throws Exception
    {
        expectError(
                "(error 3 310)",
                Code.TYPE_ERROR );
    }

    @Test
    public void includeTest() throws Exception
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

        ScreamEvaluator se = scriptEngine();

        se.evalFco( String.format( "(include \"%s\")", tf.toString() ) );

        var one = se.evalFco( "one" );
        var two = se.evalFco( "two" );

        assertEquals( i1, one );
        assertEquals( i2, two );
    }

    @Test
    public void includeNonExistingTest() throws Exception
    {
        var se = scriptEngine();

        var nonExistingName = UUID.randomUUID().toString();
        try
        {
            se.eval( String.format( "(include \"%s\")", nonExistingName ) );
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
        Scream si = new Scream();
        ScreamEvaluator se1 = (ScreamEvaluator)si.getScriptEngine();
        ScreamEvaluator se2 = (ScreamEvaluator)si.getScriptEngine();

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
    public void lockedOperation() throws Exception
    {
        expectError( "(set! + 0)", Code.CANNOT_MODIFY_CONSTANT );
    }
}
