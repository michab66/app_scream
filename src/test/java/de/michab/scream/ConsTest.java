package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

import de.michab.scream.frontend.SchemeParser;

public class ConsTest
{
    @Test
    public void basic() throws Exception
    {
        var one = SchemeInteger.createObject(1);
        var two = SchemeInteger.createObject(2);

        Cons c1 = new Cons(
                one,
                two);

        assertEquals( one, c1.getCar() );
        assertEquals( two, c1.getCdr() );
        assertFalse( c1.isProperList() );
        assertFalse( c1.isCircular() );
    }

    @Test
    public void basic2() throws Exception
    {
        var one = SchemeInteger.createObject(1);

        Cons c1 = new Cons(
                one,
                Cons.NIL);

        assertEquals( one, c1.getCar() );
        assertEquals( Cons.NIL, c1.getCdr() );
        assertTrue( c1.isProperList() );
        assertEquals( 1, c1.length() );
        assertFalse( c1.isCircular() );
    }

    @Test
    public void tail() throws Exception
    {
        Cons c1 = Cons.create(
                SchemeInteger.createObject(0),
                SchemeInteger.createObject(1),
                SchemeInteger.createObject(2),
                SchemeInteger.createObject(3),
                SchemeInteger.createObject(4)
        );

        Cons tail2 = (Cons)c1.listTail( 2 );
        assertNotNull( tail2 );
        assertEquals( SchemeInteger.createObject(2), tail2.getCar() );

        try
        {
            c1.listTail( c1.length() );
            fail();
        }
        catch ( RuntimeX e )
        {
            assertEquals( ScreamException.Code.INDEX_OUT_OF_BOUNDS, e.getCode() );
        }
    }

    @Test
    public void circular() throws Exception
    {
        Cons c1 = Cons.create(
                SchemeInteger.createObject(0),
                SchemeInteger.createObject(1),
                SchemeInteger.createObject(2),
                SchemeInteger.createObject(3),
                SchemeInteger.createObject(4)
        );

        Cons tail2 = (Cons)c1.listTail( c1.length() -1 );
        tail2.setCdr( c1 );
        assertTrue( c1.isCircular() );
    }

    @Test
    public void proper() throws Exception
    {
        var p = (Cons)
                new SchemeParser( "(1 2 3)" ).getExpression();
        assertTrue( p.isProperList() );
    }
    @Test
    public void properNot() throws Exception
    {
        var p = (Cons)
                new SchemeParser( "(1 2 3 . 4)" ).getExpression();
        assertFalse( p.isProperList() );
    }

    @Test
    public void properCircular() throws Exception
    {
        Cons c1 = Cons.create(
                SchemeInteger.createObject(0),
                SchemeInteger.createObject(1),
                SchemeInteger.createObject(2),
                SchemeInteger.createObject(3),
                SchemeInteger.createObject(4)
        );

        Cons tail2 = (Cons)c1.listTail( c1.length() -1 );
        tail2.setCdr( c1 );
        assertTrue( c1.isCircular() );
        assertFalse( c1.isProperList() );
    }

    @Test
    public void ref() throws Exception
    {
        Cons c1 = Cons.create(
                SchemeInteger.createObject(0),
                SchemeInteger.createObject(1),
                SchemeInteger.createObject(2),
                SchemeInteger.createObject(3),
                SchemeInteger.createObject(4)
        );

        var r2 = c1.listRef( 2 );
        assertNotNull( r2 );
        assertEquals( SchemeInteger.createObject(2), r2 );

        try
        {
            c1.listRef( c1.length() );
            fail();
        }
        catch ( RuntimeX e )
        {
            assertEquals( ScreamException.Code.INDEX_OUT_OF_BOUNDS, e.getCode() );
        }
    }

    @Test
    public void equality() throws Exception
    {
        Cons c1 = Cons.create(
                SchemeInteger.createObject(0),
                SchemeInteger.createObject(1),
                SchemeInteger.createObject(2),
                SchemeInteger.createObject(3),
                SchemeInteger.createObject(4)
        );
        Cons c2 = Cons.create(
                SchemeInteger.createObject(0),
                SchemeInteger.createObject(1),
                SchemeInteger.createObject(2),
                SchemeInteger.createObject(3),
                SchemeInteger.createObject(4)
        );

        assertTrue( c1.equal( c2 ) );
        assertFalse( c1.eqv( c2 ) );
        assertFalse( c1.eq( c2 ) );
    }
}
