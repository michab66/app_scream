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
import de.michab.scream.fcos.SchemeCharacter;

/**
 * r7rs 6.13.2 Ports p57
 */
public class R7rs_6_13_2_Input_Test extends ScreamBaseTest
{
    @Test
    public void read() throws Exception
    {
        expectFco(
"""
                (read (open-input-string "'elvis"))
""",
                parse( "'elvis" ) );
    }

    @Test
    public void read_char() throws Exception
    {
        expectFco(
"""
                (read-char (open-input-string "elvis"))
""",
                SchemeCharacter.createObject( 'e' ) );
    }

    @Test
    public void read_char_eof() throws Exception
    {
        expectFco(
"""
                (eof-object?
                  (let ((port (open-input-string "elvis")))
                    (read-char port) ; e
                    (read-char port) ; l
                    (read-char port) ; v
                    (read-char port) ; i
                    (read-char port) ; s
                    (read-char port) ; -> EOF
                  )
                )
""",
                bTrue );
    }

    @Test
    public void peek_char() throws Exception
    {
        expectFco(
"""
                (peek-char (open-input-string "elvis"))
""",
                SchemeCharacter.createObject( 'e' ) );
    }

    @Test
    public void read_line() throws Exception
    {
        expectFco(
"""
                (read-line (open-input-string "elvis"))
""",
                str( "elvis" ) );
    }

    @Test
    public void read_line_eof() throws Exception
    {
        expectFco(
"""
        (let ((port (open-input-string "elvis")))
          (read-line port) ; Consume elvis.
          (read-line port) ; -> EOF
        )
""",
                Port.EOF );
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
    public void eof_q() throws Exception
    {
        expectFco( "(eof-object? (eof-object))", bTrue );
        expectFco( "(eof-object? 1)", bFalse );
        expectFco( "(eof-object? 'symbol)", bFalse );
        expectFco( "(eof-object? 3.1415)", bFalse );
        expectFco( "(eof-object? #(EOF))", bFalse );
        expectFco( "(eof-object? '())", bFalse );
    }

    @Test
    public void char_ready_q() throws Exception
    {
        expectFco(
"""
                (char-ready? (open-input-string "elvis"))
""",
                bTrue );
    }

    @Test
    public void read_string() throws Exception
    {
        expectFco(
"""
                (read-string 2 (open-input-string "elvis"))
""",
                str( "el") );
    }

    private void read_u8_eof( File f ) throws Exception
    {
        expectFco( String.format(
"""
        (eof-object?
          (call-with-port
            (open-binary-input-file "%s")
            (lambda (port)
              (read-u8 port)))
        )

""", f.getPath() ),
        bTrue );
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
