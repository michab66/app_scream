/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2023 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.BufferedReader;
import java.io.Closeable;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.UUID;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Procedure;
import de.michab.scream.fcos.Bool;

/**
 * r7rs 6.13.1 Ports p56
 *
 * @author MICBINZ
 */
public class R7rs_6_13_1_Ports_Test extends ScreamBaseTest
{
    private void call_with_port( File file ) throws Exception
    {
        String uuid = UUID.randomUUID().toString();

        try ( var out = new FileWriter( file ) )
        {
            out.write( uuid );
        }

        assertTrue( file.exists() );

        expectFco(
                String.format(
"""
        (call-with-port
          (open-input-file "%s")
          (lambda (port)
            (read-line port)))

""", file.getPath(), uuid ),
                str( uuid ) );
    }
    @Test
    public void call_with_port() throws Exception
    {
        withFile( this::call_with_port );
    }

    @Test
    public void call_with_port_err_1() throws Exception
    {
        expectError( "(call-with-port)", Code.WRONG_NUMBER_OF_ARGUMENTS );
    }

    @Test
    public void call_with_port_err_2() throws Exception
    {
        RuntimeX rx = expectError( "(call-with-port 1 2)", Code.TYPE_ERROR );
        assertEquals( "port", rx.getArgument( 0 ) );
        assertEquals( "integer",rx.getArgument( 1 ) );
    }
    @Test
    public void call_with_port_err_3() throws Exception
    {
        var rx = expectError( "(call-with-port (current-input-port) 2)", Code.TYPE_ERROR );
        assertEquals( "procedure", rx.getArgument( 0 ) );
        assertEquals( "integer", rx.getArgument( 1 ) );
    }

    /**
     * p56
     */
    private void call_with_input_file( File file ) throws Exception
    {
        String uuid = UUID.randomUUID().toString();

        try ( var out = new FileWriter( file ) )
        {
            out.write( uuid );
        }

        assertTrue( file.exists() );

        expectFco(
                String.format(
"""
        (call-with-input-file
          "%s"
          (lambda (port)
            (read-line port)))

""", file.getPath(), uuid ),
                str( uuid ) );
    }
    @Test
    public void call_with_input_file() throws Exception
    {
        withFile( this::call_with_input_file );
    }

    /**
     * p56
     */
    private void call_with_output_file( File file ) throws Exception
    {
        file.delete();

        String uuid = UUID.randomUUID().toString();


        assertFalse( file.exists() );

        expectFco(
                String.format(
"""
        (call-with-output-file
          "%s"
          (lambda (port)
            (display "%s" port)))

""", file.getPath(), uuid ),
                Cons.NIL );

        try ( var in = new BufferedReader( new FileReader( file ) ) )
        {
            assertEquals( uuid, in.readLine() );
        }

    }
    /**
     * p56
     */
    @Test
    public void call_with_output_file() throws Exception
    {
        withFile( this::call_with_output_file );
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
            assertEquals( Bool.T, result );
        }
    }

    /**
     * p57
     */
    @Test
    public void open_input_bytevector() throws Exception
    {
        var se = scriptEngine();

        {
        	expectFco( 
        			se,
                """
                (define ibv (open-input-bytevector #u8(1 2 3)))
                (and (input-port? ibv) (binary-port? ibv))
                """,
               bTrue );
        }
        {
        	expectFco( 
        			se,
        			"(read-u8 ibv)",
        			i(1) );
        }
        {
        	expectFco( 
        			se,
        			"(read-u8 ibv)",
        			i(2) );
        }
        {
        	expectFco( 
        			se,
        			"(read-u8 ibv)",
        			i(3) );
        }
        {
        	expectFco( 
        			se,
        			"(eof-object? (read-u8 ibv))",
        			bTrue );
        }
    }
    public void open_input_bytevector_empty() throws Exception
    {
        var se = scriptEngine();

        {
        	expectFco( 
        			se,
                """
                (define ibv (open-input-bytevector #u8()))
                (and (input-port? ibv) (binary-port? ibv))
                """,
               bTrue );
        }
        {
        	expectFco( 
        			se,
        			"(eof-object? (read-u8 ibv))",
        			bTrue );
        }
    }
    @Test
    public void open_input_bytevector_err() throws Exception
    {
    	expectError( 
    			"(open-input-bytevector 3)",
    			Code.TYPE_ERROR );
    	expectError( 
    			"(open-input-bytevector \"string\")",
    			Code.TYPE_ERROR );
    }
}
