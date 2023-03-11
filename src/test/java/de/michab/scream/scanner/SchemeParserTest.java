/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.scanner;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamException;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Port;
import de.michab.scream.fcos.Vector;
import de.michab.scream.frontend.SchemeParser;

public class SchemeParserTest
{
    @Test
    public void parseArrayEmpty() throws ScreamException
    {
        var o = new SchemeParser( "#()" ).getExpression();
        Vector a = (Vector)o;
        assertEquals( 0, a.size() );
    }

    @Test
    public void parseArray() throws ScreamException
    {
        var o = new SchemeParser( "#(1 2 3)" ).getExpression();
        Vector a = (Vector)o;
        assertEquals( 3, a.size() );
    }

    @Test
    public void parseListEmpty() throws ScreamException
    {
        var o = new SchemeParser( "()" ).getExpression();
        assertEquals( Cons.NIL, o );
    }

    @Test
    public void parseListProper1() throws ScreamException
    {
        var o = new SchemeParser( "(a . (b . (c . (d . (e . ())))))" ).getExpression();
        Cons cons = (Cons)o;

        assertEquals( 5, cons.length() );
        assertTrue( cons.isProperList() );
    }

    @Test
    public void parseListProper2() throws ScreamException
    {
        var o = new SchemeParser( "(3 1 3)" ).getExpression();
        Cons cons = (Cons)o;

        assertEquals( 3, cons.length() );
        assertEquals( 3L, cons.listRef(0).toJava() );
        assertEquals( 1L, cons.listRef(1).toJava() );
        assertEquals( 3L, cons.listRef(2).toJava() );
        assertTrue( cons.isProperList() );
    }

    @Test
    public void parseListNotProper() throws ScreamException
    {
        var o = new SchemeParser( "(3 1 . 3)" ).getExpression();
        Cons cons = (Cons)o;

        assertFalse( cons.isProperList() );
    }

    @Test
    public void unexpectedEndOfInput()
    {
        try
        {
            // Missing closing brace.
            new SchemeParser( "(+ 300 13" ).getExpression();
            fail();
        }
        catch( RuntimeX e )
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
            new SchemeParser( ")" ).getExpression();
            fail();
        }
        catch( RuntimeX e )
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
        catch( RuntimeX e )
        {
            // Unexpected end of input.
            assertEquals( 37, e.getId() );
        }
    }
}
