/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamException.Code;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.frontend.SchemeParser;

public class ConsTest extends ScreamBaseTest
{
    @Test
    public void constantNil() throws Exception
    {
        Cons c = Cons.NIL;

        assertTrue( FirstClassObject.isConstant( c ) );
    }

    @Test
    public void constantGeneral() throws Exception
    {
        var c = parse( "(3 1 3)" );
        assertFalse( FirstClassObject.isConstant( c ) );
        c = FirstClassObject.setConstant( c );
        assertTrue( FirstClassObject.isConstant( c ) );
    }

    @Test
    public void basic() throws Exception
    {
        var one = i1;
        var two = i2;

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
        var one = i1;

        Cons c1 = new Cons(
                one,
                Cons.NIL);

        assertEquals( one, c1.getCar() );
        assertEquals( Cons.NIL, c1.getCdr() );
        assertTrue( c1.isProperList() );
        assertEquals( 1, c1.length() );
        assertEquals( 1, c1.properLength() );
        assertFalse( c1.isCircular() );
    }

    @Test
    public void consCreate() throws Exception
    {
        // Tests the create call.
        Cons c1 = Cons.create(
                i(0),
                i(1),
                i(2),
                i(3),
                i(4) );

        assertNotNull( c1 );
        assertEquals( 5, c1.length() );
        assertTrue( c1.isProperList() );
        assertFalse( c1.isCircular() );

        int count = 0;
        while ( c1 != Cons.NIL )
        {
            assertEquals( i(count), c1.getCar() );
            count++;
            c1 = (Cons)c1.getCdr();
        }
    }

    @Test
    public void tail() throws Exception
    {
        Cons c1 = readSingleExpression(
                "(0 1 2 3 4)",
                Cons.class );

        Cons tail2 = c1.listTail( 2 );
        assertNotNull( tail2 );
        assertEquals( i(2), tail2.getCar() );
        assertEquals( 3, tail2.length() );
    }

    @Test
    public void tailEmpty() throws Exception
    {
        Cons c1 = readSingleExpression(
                "(0 1 2 3 4)",
                Cons.class );

        Cons tail1 = c1.listTail( c1.length() );
        assertEquals( Cons.NIL, tail1 );
    }

    @Test
    public void proper() throws Exception
    {
        new SchemeParser( "(1 2 3)" ).getExpression();
    }

    @Test
    public void properNot() throws Exception
    {
        var p = readSingleExpression( "(1 2 3 . 4)", Cons.class );
        assertFalse( p.isProperList() );
        assertFalse( p.isCircular() );
        assertEquals( 3, p.length() );
        try
        {
            p.properLength();
            fail();
        }
        catch ( RuntimeX e) {
            assertEquals( Code.EXPECTED_PROPER_LIST, e.getCode() );
        }
    }

    @Test
    public void circular() throws Exception
    {
        Cons c1 = readSingleExpression(
                "(0 1 2 3 4)",
                Cons.class );

        Cons tail2 = c1.listTail( c1.length() -1 );
        tail2.setCdr( c1 );
        assertTrue( c1.isCircular() );
        assertFalse( c1.isProperList() );
    }

    @Test
    public void ref() throws Exception
    {
        Cons c1 = readSingleExpression(
                "(0 1 2 3 4)",
                Cons.class );

        var r2 = c1.listRef( 2 );
        assertNotNull( r2 );
        assertEquals( i(2), r2 );

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
        Cons c1 = readSingleExpression(
                "(0 1 2 3 4)",
                Cons.class );
        Cons c2 = readSingleExpression(
                "(0 1 2 3 4)",
                Cons.class );

        assertTrue( FirstClassObject.equal( c1, c2 ) );
        assertFalse( FirstClassObject.eqv( c1, c2 ) );
        assertFalse( FirstClassObject.eq( c1, c2 ) );
    }

    @Test
    public void eval() throws Exception
    {
        ScreamEvaluator se = scriptEngine();
        var env = se.getInteraction();
        Cons cons = readSingleExpression( "(+ 1 2)", Cons.class );

        FirstClassObject result =  Scream.toStack(
                c -> cons.evaluate( env, c ) );

        assertEqualq( ScreamBaseTest.i3, result );
    }

    @Test
    public void evalErr() throws Exception
    {
        ScreamEvaluator se = scriptEngine();
        var env = se.getInteraction();
        Cons cons = readSingleExpression( "(0 1 2)", Cons.class );
        try
        {
            Scream.toStack(
                    c -> cons.evaluate( env, c ) );
            fail();
        }
        catch ( ScreamException e )
        {
            assertEquals( Code.CALLED_NON_PROCEDURAL, e.getCode() );
        }
    }

    @Test
    public void evalErrNil() throws Exception
    {
        ScreamEvaluator se = scriptEngine();
        var env = se.getInteraction();
        Cons cons = readSingleExpression( "(() 1 2)", Cons.class );
        try
        {
            Scream.toStack(
                    c -> cons.evaluate( env, c ) );
            fail();
        }
        catch ( ScreamException e )
        {
            assertEquals( Code.CALLED_NON_PROCEDURAL, e.getCode() );
        }
    }
}
