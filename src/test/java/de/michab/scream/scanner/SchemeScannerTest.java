/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.scanner;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.StringReader;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamException.Code;
import de.michab.scream.frontend.SchemeScanner7;
import de.michab.scream.frontend.Token;

public class SchemeScannerTest
{
    @Test
    public void empty() throws Exception
    {
        StringReader input = new StringReader( "" );

        SchemeScanner7 s = new SchemeScanner7( input );

        var t = s.getNextToken();

        assertEquals( Token.Tk.Eof, t.getType() );
    }

    @Test
    public void character_plus() throws Exception
    {
        StringReader input = new StringReader( "#\\+" );

        SchemeScanner7 s = new SchemeScanner7( input );

        var t = s.getNextToken();

        assertEquals( Token.Tk.Char, t.getType() );
        assertEquals( '+', t.characterValue() );
    }
    @Test
    public void character_lambda() throws Exception
    {
        StringReader input = new StringReader( "#\\λ" );

        SchemeScanner7 s = new SchemeScanner7( input );

        var t = s.getNextToken();

        assertEquals( Token.Tk.Char, t.getType() );
        assertEquals( 'λ', t.characterValue() );
    }
    @Test
    public void characterHex_lambda() throws Exception
    {
        StringReader input = new StringReader( "#\\x03BB" );

        SchemeScanner7 s = new SchemeScanner7( input );

        var t = s.getNextToken();

        assertEquals( Token.Tk.Char, t.getType() );
        assertEquals( 'λ', t.characterValue() );
    }
    @Test
    public void characterHex_ffff1() throws Exception
    {
        StringReader input = new StringReader( "#\\xFFFF1" );

        SchemeScanner7 s = new SchemeScanner7( input );

        try
        {
          s.getNextToken();
          fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.SCAN_UNEXPECTED_CHAR, rx.getCode() );
            assertEquals( "#\\xFFFF1", rx.getArgument( 2 ) );
        }
    }
    @Test
    public void characterHex_syntax() throws Exception
    {
        // Actually matched as a bad character-name followed by number 1.
        StringReader input = new StringReader( "#\\xFFFyF1" );

        SchemeScanner7 s = new SchemeScanner7( input );

        try
        {
          s.getNextToken();
          fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.SCAN_UNEXPECTED_CHAR, rx.getCode() );
            assertEquals( "#\\xFFFyF", rx.getArgument( 2 ) );
        }
    }
    @Test
    public void characterName_space() throws Exception
    {
        StringReader input = new StringReader( "#\\space" );

        SchemeScanner7 s = new SchemeScanner7( input );

        var t = s.getNextToken();

        assertEquals( Token.Tk.Char, t.getType() );
        assertEquals( ' ', t.characterValue() );
    }
    @Test
    public void characterName_error() throws Exception
    {
        StringReader input = new StringReader( "#\\error" );

        SchemeScanner7 s = new SchemeScanner7( input );

        try
        {
          s.getNextToken();
          fail();
        }
        catch ( RuntimeX rx )
        {
          assertEquals( Code.SCAN_UNEXPECTED_CHAR, rx.getCode() );
          assertEquals( "#\\error", rx.getArgument( 2 ) );
        }
    }

    @Test
    public void symbol() throws Exception
    {
        StringReader input = new StringReader( "+" );

        SchemeScanner7 s = new SchemeScanner7( input );

        var t = s.getNextToken();

        assertEquals( Token.Tk.Symbol, t.getType() );
        assertEquals( "+", t.stringValue() );
    }

    @Test
    public void vector() throws Exception
    {
        StringReader input = new StringReader( "#(" );

        SchemeScanner7 s = new SchemeScanner7( input );

        var t = s.getNextToken();

        assertEquals( Token.Tk.Array, t.getType() );
    }

    @Test
    public void byteVector() throws Exception
    {
        StringReader input = new StringReader( " #u8(" );

        SchemeScanner7 s = new SchemeScanner7( input );

        var t = s.getNextToken();

        assertEquals( Token.Tk.Bytevector, t.getType() );
    }
}
