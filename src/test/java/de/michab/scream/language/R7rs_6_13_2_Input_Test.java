/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2023 Michael G. Binz
 */
package de.michab.scream.language;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
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
    public void eof_object() throws Exception
    {
        expectFco(
"""
                (eof-object)
""",
                s( "EOF" ) );
    }

    @Test
    public void eof_q() throws Exception
    {
        expectFco(
"""
                (eof-object? (eof-object))
""",
                bTrue );
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
}
