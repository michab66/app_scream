/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2023 Michael G. Binz
 */
package de.michab.scream.language;

import java.io.File;
import java.io.FileOutputStream;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Port;

/**
 * r7rs 6.13.2 Ports p57
 */
public class R7rs_6_13_2_Input_Test extends ScreamBaseTest
{
    @Test
    public void read() throws Exception
    {
        var t = makeTester();

        t.execute(
                "(define port (open-input-string \"'elvis\"))" );
        t.expectFco(
                "(read port)",
                "'elvis" );
        t.expectFco(
                "(eof-object? (read port))",
                bTrue );
        t.execute(
                "(close-input-port port)" );
    }

    @Test
    public void read_char() throws Exception
    {
        var t = makeTester();

        t.execute(
                "(define port (open-input-string \"elvis\"))" );
        t.expectFco(
                "(read-char port)",
                "#\\e" );
        t.expectFco(
                "(read-char port)",
                "#\\l" );
        t.expectFco(
                "(read-char port)",
                "#\\v" );
        t.expectFco(
                "(read-char port)",
                "#\\i" );
        t.expectFco(
                "(read-char port)",
                "#\\s" );
        t.expectFco(
                "(eof-object? (read-char port))",
                bTrue );
        t.execute(
                "(close-input-port port)" );
    }

    @Test
    public void peek_char() throws Exception
    {
        var t = makeTester();

        t.execute(
                "(define port (open-input-string \"elvis\"))" );

        t.expectFco(
                "(peek-char port)",
                "#\\e" );
        t.expectFco(
                "(peek-char port)",
                "#\\e" );
        t.expectFco(
                "(read-char port)",
                "#\\e" );
        t.expectFco(
                "(read-char port)",
                "#\\l" );
        t.execute(
                "(close-input-port port)" );
    }

    @Test
    public void read_line_eof() throws Exception
    {
        var t = makeTester();

        t.execute(
                "(define port (open-input-string \"elvis\"))" );
        t.expectFco(
                "(read-line port)",
                "\"elvis\"" );
        t.expectFco(
                "(eof-object? (read-line port))",
                bTrue );
        t.execute(
                "(close-input-port port)" );
    }

    @Test
    public void eof_object() throws Exception
    {
        expectFco(
"""
                (eof-object)
""",
                Port.EOF );
    }

    @Test
    public void eofQ() throws Exception
    {
        expectFco( "(eof-object? (eof-object))", bTrue );
        expectFco( "(eof-object? 1)", bFalse );
        expectFco( "(eof-object? 'symbol)", bFalse );
        expectFco( "(eof-object? 3.1415)", bFalse );
        expectFco( "(eof-object? #(EOF))", bFalse );
        expectFco( "(eof-object? '())", bFalse );
    }

    @Test
    public void char_readyQ() throws Exception
    {
        var t = makeTester();

        t.execute(
                "(define port (open-input-string \"e\"))" );
        t.expectFco(
                "(char-ready? port)",
                bTrue );
        t.expectFco(
                "(read-char port)",
                "#\\e" );
        // So it is specified (r7rs.p58):
        t.expectFco(
                "(char-ready? port)",
                bTrue );
        t.expectFco(
                "(eof-object? (read-char port))",
                bTrue );
        t.execute(
                "(close-input-port port)" );
    }

    @Test
    public void read_string() throws Exception
    {
        var t = makeTester();

        t.execute(
                "(define port (open-input-string \"elvis\"))" );
        t.expectFco(
                "(read-string 2 port)",
                str( "el") );
        t.expectFco(
                "(read-string 2 port)",
                str( "vi") );
        t.expectFco(
                "(read-string 2 port)",
                str( "s") );
        t.expectFco(
                "(eof-object? (read-string 2 port))",
                bTrue );
        t.execute(
                "(close-input-port port)" );
    }

    @Test
    public void read_u8_eof() throws Exception
    {
        withFile( this::read_u8 );
    }

    private void read_u8( File f ) throws Exception
    {
        try ( var os = new FileOutputStream( f ) )
        {
            os.write( 1 );

        }
        expectFco( String.format(
"""
          (call-with-port
            (open-binary-input-file "%s")
            (lambda (port)
              (read-u8 port)))

""", f.getPath() ),
        i1 );
    }

    @Test
    public void read_u8() throws Exception
    {
        withFile( this::read_u8 );
    }

    private void peek_u8( File f ) throws Exception
    {
        try ( var os = new FileOutputStream( f ) )
        {
            os.write( 1 );

        }
        expectFco( String.format(
"""
          (call-with-port
            (open-binary-input-file "%s")
            (lambda (port)
              (peek-u8 port)))

""", f.getPath() ),
        i1 );
    }

    @Test
    public void peek_u8() throws Exception
    {
        withFile( this::peek_u8 );
    }

    private void u8_ready_q( File f ) throws Exception
    {
        try ( var os = new FileOutputStream( f ) )
        {
            os.write( 1 );

        }
        expectFco( String.format(
"""
          (call-with-port
            (open-binary-input-file "%s")
            (lambda (port)
              (u8-ready? port)))

""", f.getPath() ),
        bTrue );
    }

    @Test
    public void u8_ready_q() throws Exception
    {
        withFile( this::u8_ready_q );
    }
}
