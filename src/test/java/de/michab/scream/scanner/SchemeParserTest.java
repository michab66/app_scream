/* $Id: SchemeParser.java 172 2009-03-19 21:21:48Z Michael $
 *
 * Scream / Frontend
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2009 Michael G. Binz
 */
package de.michab.scream.scanner;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

import de.michab.scream.Port;
import de.michab.scream.ScreamException;
import de.michab.scream.frontend.FrontendX;
import de.michab.scream.frontend.SchemeParser;

public class SchemeParserTest
{
    @Test
    public void unexpectedEndOfInput()
    {
        try
        {
            // Missing closing brace.
            SchemeParser sp = new SchemeParser( "(+ 300 13" );
            var x = sp.getExpression();
            fail();
        }
        catch( FrontendX e )
        {
            assertEquals(
                    ScreamException.Code.PARSE_UNEXPECTED_EOF,
                    e.getCode() );
        }
    }

    @Test
    public void unbalanced()
    {
        try
        {
            // Missing closing brace.
            SchemeParser sp = new SchemeParser( ")" );
            var x = sp.getExpression();
            fail();
        }
        catch( FrontendX e )
        {
            assertEquals(
                    ScreamException.Code.PARSE_UNEXPECTED,
                    e.getCode() );
        }
    }

    @Test
    public void biExpression()
    {
        var x1 = "(+ 300 13)";
        var x2 = "(+ 4 5)";

        try
        {
            SchemeParser sp = new SchemeParser( x1 + x2 );
            var x = sp.getExpression();
            assertEquals( x1, x.toString() );
            var y = sp.getExpression();
            assertEquals( x2, y.toString() );
            var z = sp.getExpression();
            assertEquals( Port.EOF, z );
        }
        catch( FrontendX e )
        {
            // Unexpected end of input.
            assertEquals( 37, e.getId() );
        }
    }
}
