/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2023 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.StringWriter;
import java.nio.file.Files;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.SchemeString;

/**
 * r7rs 6.13.1 Ports
 *
 * @author MICBINZ
 */
public class R7rs_6_13_3_Output_Test extends ScreamBaseTest
{
    /**
     * p59
     */
    @Test
    public void display() throws Exception
    {
        var se = scriptEngine();

        var sw = new StringWriter();

        {
            var context = se.getContext();
            context.setWriter( sw );
            se.setContext( context );
        }

        assertEqualq(
                se.evalFco( "(display 'donald)" ),
                Cons.NIL );

        assertEquals( "donald", sw.toString() );
    }

    /**
     * p59
     */
    @Test
    public void display_2() throws Exception
    {
        var se = scriptEngine();

        var sw = new StringWriter();

        {
            var context = se.getContext();
            context.setWriter( sw );
            se.setContext( context );
        }

        assertEqualq(
                se.evalFco(
"""
                        (display 'donald)
                        (display " ")
                        (display 'duck)
""" ),
                Cons.NIL );

        assertEquals( "donald duck", sw.toString() );
    }

    /**
     * p59
     */
    @Test
    public void display_with_port_2() throws Exception
    {
        File f = tmpFile( getClass() );

        assertTrue( f.exists() );
        f.delete();
        assertFalse( f.exists() );

        try
        {
        var se = scriptEngine();

        assertEqualq(
                se.evalFco( String.format(
"""
                    (let ((of (open-output-file "%s")))
                        (display 'donald of)
                        (display " " of)
                        (display 'duck of)
                        (close-output-port of))
""" , f.getPath() ) ),
                Cons.NIL );

        assertTrue( f.exists() );
        assertEquals( "donald duck", Files.readString( f.toPath() ) );
        }
        finally
        {
            f.delete();
            assertFalse( f.exists() );
        }
    }

    /**
     * p59
     */
    @Test
    public void newline__() throws Exception
    {
        var se = scriptEngine();

        var sw = new StringWriter();

        {
            var context = se.getContext();
            context.setWriter( sw );
            se.setContext( context );
        }

        expectFco(
                se,
                """
                (newline)
                """ ,
                Cons.NIL );

        assertEquals( "\n", sw.toString() );
    }

    /**
     * p59
     */
    @Test
    public void newline__port() throws Exception
    {
        expectFco(
            """
            (define os (open-output-string))
            (newline os)
            (let ((result (get-output-string os)))
              (close-port os)
              result)
            """,
            SchemeString.makeEscaped( "\n" )
            );
    }

    /**
     * p59
     */
    @Test
    public void write_string__() throws Exception
    {
        var se = scriptEngine();

        var sw = new StringWriter();

        {
            var context = se.getContext();
            context.setWriter( sw );
            se.setContext( context );
        }

        assertEqualq(
                se.evalFco(
                        """
                        (write-string "donald")
                        """ ),
                Cons.NIL );

        assertEquals( "donald", sw.toString() );
    }

    /**
     * p59
     */
    @Test
    public void write_string__port() throws Exception
    {
        expectFco(
            """
            (define os (open-output-string))
            (write-string "donald" os)
            (let ((result (get-output-string os)))
              (close-port os)
              result)
            """,
            """
            "donald"
            """);
    }

    /**
     * p59
     */
    @Test
    public void write_string__port_start() throws Exception
    {
        expectFco(
            """
            (define os (open-output-string))
            (write-string "donald" os 1)
            (let ((result (get-output-string os)))
              (close-port os)
              result)
            """,
            """
            "onald"
            """ );
    }

    /**
     * p59
     */
    @Test
    public void write_string__port_start_end() throws Exception
    {
        expectFco(
            """
            (define os (open-output-string))
            (write-string "donald" os 1 5)
            (let ((result (get-output-string os)))
              (close-port os)
              result)
            """,
            """
            "onal"
            """ );
    }
}
