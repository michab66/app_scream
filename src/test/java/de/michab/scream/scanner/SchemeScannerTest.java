/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream.scanner;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.io.StringReader;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Int;
import de.michab.scream.fcos.Real;
import de.michab.scream.frontend.SchemeScanner7;
import de.michab.scream.frontend.Token;
import de.michab.scream.frontend.Token.Tk;

public class SchemeScannerTest extends ScreamBaseTest
{
    private SchemeScanner7 makeScanner( String script )
    {
        return new SchemeScanner7(
                new StringReader( script ),
                getClass().getSimpleName() );
    }

    /**
     * Returns the next token from the passed script text and checks
     * if this is followed by EOF.
     *
     * @param script The script to scan.
     * @return The next token.
     */
    private Token toToken( String script ) throws RuntimeX, IOException
    {
        var scanner = makeScanner( script );
        var result = scanner.getNextToken();
        // Ensure that the whole expression got consumed.
        assertEquals( Token.Tk.Eof, scanner.getNextToken().getType() );

        return result;
    }

    @Test
    public void empty() throws Exception
    {
        // Note that toToken implicitly checks the follow-up token.
        // In this special case that ensures that the scanner
        // returns Tk.Eof repeatedly.

        var t = toToken( "" );

        assertEquals( Token.Tk.Eof, t.getType() );
    }

    @Test
    public void character_plus() throws Exception
    {
        var t = toToken( "#\\+" );

        assertEquals( Token.Tk.Char, t.getType() );
        assertEquals( '+', t.characterValue() );
    }

    @Test
    public void character_lambda() throws Exception
    {
        var t = toToken( "#\\λ" );

        assertEquals( Token.Tk.Char, t.getType() );
        assertEquals( 'λ', t.characterValue() );
    }
    @Test
    public void characterHex_lambda() throws Exception
    {
        var t = toToken( "#\\x03BB" );

        assertEquals( Token.Tk.Char, t.getType() );
        assertEquals( 'λ', t.characterValue() );
    }
    @Test
    public void characterHex_ffff1() throws Exception
    {
        try
        {
          toToken( "#\\xFFFF1" );
          fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.SCAN_UNEXPECTED_CHAR, rx.getCode() );
            assertEquals(
                    str( "#\\xFFFF1" ), rx.getArgument( 2 ) );
        }
    }
    @Test
    public void characterHex_syntax() throws Exception
    {
        try
        {
            // Actually matched as a bad character-name followed by number 1.
            toToken( "#\\xFFFyF1" );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.SCAN_UNEXPECTED_CHAR, rx.getCode() );
            assertEquals( str("#\\xFFFyF"), rx.getArgument( 2 ) );
        }
    }
    @Test
    public void characterName_space() throws Exception
    {
        var t = toToken( "#\\space" );

        assertEquals( Token.Tk.Char, t.getType() );
        assertEquals( ' ', t.characterValue() );
    }
    @Test
    public void characterName_error() throws Exception
    {
        try
        {
            toToken( "#\\error" );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.SCAN_UNEXPECTED_CHAR, rx.getCode() );
            assertEquals(
                    str("#\\error"),
                    rx.getArgument( 2 ) );
        }
    }

    @Test
    public void symbol() throws Exception
    {
        var t = toToken( "+" );

        assertEquals( Token.Tk.Symbol, t.getType() );
        assertEquals( "+", t.stringValue() );
    }

    @Test
    public void vector() throws Exception
    {
        var t = toToken( "#(" );

        assertEquals( Token.Tk.Array, t.getType() );
    }

    @Test
    public void byteVector() throws Exception
    {
        var t = toToken( " #u8(" );

        assertEquals( Token.Tk.Bytevector, t.getType() );
    }

    @Test
    public void lineComment_plain() throws Exception
    {
        var t = toToken(
                ";  #| special comment characters  # | |# \n 313" );

        assertEquals( Token.Tk.Number, t.getType() );
        assertEquals( 313, t.numberValue().asLong() );
    }

    @Test
    public void datumComment_plain() throws Exception
    {
        var t = toToken(
                "#;" );

        assertEquals( Tk.DatumComment, t.getType() );
    }

    @Test
    public void nestedComment_plain() throws Exception
    {
        var t = toToken(
                "#| special comment characters  # | |# 313" );

        assertEquals( Token.Tk.Number, t.getType() );
    }

    @Test
    public void nestedComment_plain_multiline() throws Exception
    {
        var t = toToken(
"""
#|
 | Durch diese hohle Gasse muss er kommen.
 |# 313
""" );

        assertEquals( Token.Tk.Number, t.getType() );
    }

    @Test
    public void nestedComment_nested() throws Exception
    {
        var t = toToken(
                "#| #| #| #| #| #| #|  |<-comment-># | |# |# |# |# |# |# |# 313" );

        assertEquals( Token.Tk.Number, t.getType() );
    }

    @Test
    public void nestedComment_error_unclosed() throws Exception
    {
        try
        {
            toToken( "#|  #| b |# ... 313" );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.SCAN_UNBALANCED_COMMENT, rx.getCode() );
        }
    }

    @Test
    public void nestedComment_error_unopened() throws Exception
    {
        try
        {
            toToken( " |# 313 " );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.SCAN_UNBALANCED_COMMENT, rx.getCode() );
        }
    }

    @Test
    public void nestedComment_error_doublyClosed() throws Exception
    {
        try
        {
            toToken( "#| comment |# |# 313 " );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.SCAN_UNBALANCED_COMMENT, rx.getCode() );
        }
    }

    private void testInteger( String code, long expected ) throws Exception
    {
        var t = toToken( code );

        assertEquals( Token.Tk.Number, t.getType() );
        assertInstanceOf( Int.class, t.numberValue() );
        assertEquals( expected, t.numberValue().asLong() );
    }

    @Test
    public void numberInteger() throws Exception
    {
        testInteger( "313", 313 );
        testInteger( "#e313", 313 );
        testFloat( "#i313", 313 );
        testInteger( "-313", -313 );

        testInteger( "#b10", 2 );
        testInteger( "#b-10", -2 );
        testInteger( "#b#e10", 2 );
        testInteger( "#b#e-10", -2 );
        testFloat( "#b#i10", 2 );
        testInteger( "#e#b10", 2 );
        testFloat( "#i#b10", 2 );

        testInteger( "#o10", 8 );
        testInteger( "#o-10", -8 );
        testInteger( "#o#e10", 8 );
        testInteger( "#o#e-10", -8 );
        testFloat( "#o#i10", 8 );
        testInteger( "#e#o10", 8 );
        testFloat( "#i#o10", 8 );

        testInteger( "#d10", 10 );
        testInteger( "#d-10", -10 );
        testInteger( "#d#e10", 10 );
        testInteger( "#d#e-10", -10 );
        testFloat( "#d#i10", 10 );
        testInteger( "#e#d10", 10 );
        testFloat( "#i#d10", 10 );

        testInteger( "#xba", 186 );
        testInteger( "#xBa", 186 );
        testInteger( "#xbA", 186 );
        testInteger( "#xBA", 186 );

        testInteger( "#x10", 16 );
        testInteger( "#x10", 16 );
        testInteger( "#x-10", -16 );
        testInteger( "#x#e10", 16 );
        testInteger( "#x#e-10", -16 );
        testFloat( "#x#i10", 16 );
        testInteger( "#e#x10", 16 );
        testFloat( "#i#x10", 16 );
    }

    private void testFloat( String code, double expected ) throws Exception
    {
        var t = toToken( code );

        assertEquals( Token.Tk.Number, t.getType() );
        assertInstanceOf( Real.class, t.numberValue() );
        assertEquals( expected, t.numberValue().asDouble() );
    }

    @Test
    public void numberDouble() throws Exception
    {
        testFloat( "313e0", 313d );
        testFloat( "313.0", 313d );
        testFloat( "-313.0", -313d );
        testFloat( "313.", 313d );
        testFloat( ".313", .313d );
        testFloat( "-.313", -.313d );
        testFloat( "-313.", -313d );
        testFloat( "-313.0e1", -3130d );
        testFloat( "-313.0e-1", -31.3d );
        testFloat( "-313.0e2", -31300d );
        testFloat( ".313e3", 313d );
    }

    @Test
    public void numberDoubleExact() throws Exception
    {
        // Default.  Exactness not specified.
        {
            var t = toToken( "313.0" );
            assertEquals( Tk.Number, t.getType() );
            assertInstanceOf( Real.class, t.numberValue() );
            // By default inexact.
            assertFalse( t.numberValue().isExact() );
        }
        // Explicitly exact.
        {
            var t = toToken( "#e313.0" );
            assertEquals( Tk.Number, t.getType() );
            assertInstanceOf( Int.class, t.numberValue() );
            assertTrue( t.numberValue().isExact() );
        }
        // Explicitly inexact.
        {
            var t = toToken( "#i313.0" );
            assertEquals( Tk.Number, t.getType() );
            assertInstanceOf( Real.class, t.numberValue() );
            assertFalse( t.numberValue().isExact() );
        }
    }

    @Test
    public void numberIntegerExact() throws Exception
    {
        // Default.  Exactness not specified.
        {
            var t = toToken( "313" );
            assertEquals( Tk.Number, t.getType() );
            assertInstanceOf( Int.class, t.numberValue() );
            // By default exact.
            assertTrue( t.numberValue().isExact() );
        }
        // Explicitly exact.
        {
            var t = toToken( "#e313" );
            assertEquals( Tk.Number, t.getType() );
            assertInstanceOf( Int.class, t.numberValue() );
            assertTrue( t.numberValue().isExact() );
        }
        // Explicitly inexact.
        {
            var t = toToken( "#i313" );
            assertEquals( Tk.Number, t.getType() );
            assertInstanceOf( Real.class, t.numberValue() );
            assertFalse( t.numberValue().isExact() );
        }
    }
}
