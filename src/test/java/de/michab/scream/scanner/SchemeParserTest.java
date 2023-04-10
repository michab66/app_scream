/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.scanner;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.StringReader;

import org.junit.jupiter.api.Test;

import de.michab.scream.frontend.SchemeScanner7;
import de.michab.scream.frontend.Token;

public class SchemeParserTest
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
}
