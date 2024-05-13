/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2023 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.BufferedReader;
import java.io.Closeable;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.UUID;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Cons;

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
    public void call_with_port_err() throws Exception
    {
        var t = makeTester();

        t.expectError(
                "(call-with-port)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );

        {
            var rx = t.expectError(
                    "(call-with-port 1 2)",
                    Code.TYPE_ERROR );
            assertEquals(
                    str( "port" ),
                    rx.getArgument( 0 ) );
            assertEquals(
                    str( "integer" ),
                    rx.getArgument( 1 ) );
            assertEqualq(
                    s("call-with-port"),
                    rx.getOperationName() );
        }
        {
            var rx = expectError(
                    "(call-with-port (current-input-port) 2)",
                    Code.TYPE_ERROR );
            assertEquals(
                    str( "procedure" ),
                    rx.getArgument( 0 ) );
            assertEquals(
                    str( "integer" ) ,
                    rx.getArgument( 1 ) );
            assertEqualq(
                    s("call-with-port"),
                    rx.getOperationName() );
        }
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

    @Test
    public void call_with_input_file_err() throws Exception
    {
        var t = makeTester();

        t.expectError(
                "(call-with-input-file)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );

        {
            var rx = t.expectError(
                    "(call-with-input-file 1 2)",
                    Code.TYPE_ERROR );
            assertEquals(
                    str( "string" ),
                    rx.getArgument( 0 ) );
            assertEquals(
                    str( "integer" ),
                    rx.getArgument( 1 ) );
            assertEqualq(
                    s("call-with-input-file"),
                    rx.getOperationName() );
        }
        {
            var rx = expectError(
                    "(call-with-input-file \"tmp\" 2)",
                    Code.TYPE_ERROR );
            assertEquals(
                    str( "procedure" ),
                    rx.getArgument( 0 ) );
            assertEquals(
                    str( "integer" ) ,
                    rx.getArgument( 1 ) );
            assertEqualq(
                    s("call-with-input-file"),
                    rx.getOperationName() );
        }
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
    public void portQ() throws Exception
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
    public void close_port() throws Exception
    {
        var t = makeTester();

        assertEqualq(
                s( "close-port" ),
                t.expectError(
                        "(close-port 0)",
                        Code.TYPE_ERROR ).getOperationName() );
    }

    /**
     * p56
     */
    @Test
    public void close_input_port() throws Exception
    {
        var t = makeTester();

        assertEqualq(
                s( "close-input-port" ),
                t.expectError(
                        "(close-input-port 0)",
                        Code.TYPE_ERROR ).getOperationName() );
    }

    /**
     * p56
     */
    @Test
    public void close_output_port() throws Exception
    {
        var t = makeTester();

        assertEqualq(
                s( "close-output-port" ),
                t.expectError(
                        "(close-output-port 0)",
                        Code.TYPE_ERROR ).getOperationName() );
    }

    /**
     * p57
     */
    @Test
    public void open_input_bytevector() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                """
                (define ibv (open-input-bytevector #u8(1 2 3)))
                (and (input-port? ibv) (binary-port? ibv))
                """,
                bTrue );
        t.expectFco(
                "(read-u8 ibv)",
                i(1) );
        t.expectFco(
                "(read-u8 ibv)",
                i(2) );
        t.expectFco(
                "(read-u8 ibv)",
                i(3) );
        t.expectFco(
                "(eof-object? (read-u8 ibv))",
                bTrue );
        t.execute(
                "(close-port ibv)" );
    }

    @Test
    public void open_input_bytevector_empty() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                """
                (define ibv (open-input-bytevector #u8()))
                (and (input-port? ibv) (binary-port? ibv))
                """,
                bTrue );
        t.expectFco(
                "(eof-object? (read-u8 ibv))",
                bTrue );
        t.execute(
                "(close-port ibv)" );
    }

    @Test
    public void open_input_bytevector_err() throws Exception
    {
        var t = makeTester();

    	assertEqualq(
    	        s("open-input-bytevector"),
    	        t.expectError(
    	                "(open-input-bytevector 3)",
    	                Code.TYPE_ERROR ).getOperationName() );
    }
}
