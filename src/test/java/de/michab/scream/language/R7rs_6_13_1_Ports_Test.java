/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2023 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import java.io.Closeable;
import java.io.File;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Procedure;
import de.michab.scream.fcos.SchemeBoolean;

/**
 * r7rs 6.13.1 Ports p56
 *
 * @author MICBINZ
 */
public class R7rs_6_13_1_Ports_Test extends ScreamBaseTest
{
    /**
     * p56
     */
    @Test
    public void call_with_port() throws Exception
    {
    }

    /**
     * p56
     */
    @Test
    public void call_with_input_file() throws Exception
    {
    }

    /**
     * p56
     */
    @Test
    public void call_with_output_file() throws Exception
    {
    }


    /**
     * p56
     */
    @Test
    public void input_port_Q() throws Exception
    {
        expectFco( "(input-port? (current-input-port))", bTrue );
        expectFco( "(input-port? (current-output-port))", bFalse );
        expectFco( "(input-port? 313)", bFalse );
        expectFco( "(input-port? '())", bFalse );
    }
    /**
     * p56
     */
    @Test
    public void output_port_Q() throws Exception
    {
        expectFco( "(output-port? (current-input-port))", bFalse );
        expectFco( "(output-port? (current-output-port))", bTrue );
        expectFco( "(output-port? 313)", bFalse );
        expectFco( "(output-port? '())", bFalse );
    }
    /**
     * p56
     */
    @Test
    public void textual_port_Q() throws Exception
    {
        expectFco( "(textual-port? (current-input-port))", bTrue );
        expectFco( "(textual-port? (current-output-port))", bTrue );
        expectFco( "(textual-port? 313)", bFalse );
        expectFco( "(textual-port? '())", bFalse );
    }
    /**
     * p56
     */
    @Test
    public void binary_port_Q() throws Exception
    {
        expectFco( "(binary-port? (current-input-port))", bFalse );
        expectFco( "(binary-port? (current-output-port))", bFalse );
        expectFco( "(binary-port? 313)", bFalse );
        expectFco( "(binary-port? '())", bFalse );
    }

    private Closeable binary_port_Q2( File f ) throws Exception
    {
        expectFco( String.format(
"""
        (define bp (open-binary-input-file "%s"))
        (binary-port? bp)
""", f.getPath() ),
        bTrue );

        return null;
    }
    @Test
    @Disabled( "needs open-binary-input-file" )
    public void binary_port_Q2() throws Exception
    {
        withFile( this::binary_port_Q2 );
    }

    /**
     * p56
     */
    @Test
    public void port_Q() throws Exception
    {
        expectFco( "(port? (current-input-port))", bTrue );
        expectFco( "(port? (current-output-port))", bTrue );
        expectFco( "(port? (current-error-port))", bTrue );
        expectFco( "(port? 1)", bFalse );
        expectFco( "(port? ())", bFalse );
        expectFco( "(port? 'donald)", bFalse );
    }

    /**
     * p56
     */
    @Test
    public void basic_1() throws Exception
    {
        var se = scriptEngine();

        {
            var result = se.evalFco(
                """
                current-input-port
                """ );
            assertInstanceOf( Procedure.class, result );
        }
        {
            var result = se.evalFco(
                """
                (port? (current-input-port))
                """ );
            assertEquals( SchemeBoolean.T, result );
        }
    }

}
