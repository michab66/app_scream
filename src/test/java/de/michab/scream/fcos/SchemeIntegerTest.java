/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamException;
import de.michab.scream.ScreamException.Code;

public class SchemeIntegerTest extends ScreamBaseTest
{
    private boolean equal( FirstClassObject a, FirstClassObject b )
    {
        var r1 = FirstClassObject.equal( a, b );
        var r2 = FirstClassObject.equal( b, a );
        assertEquals( r1, r2 );
        return r1;
    }

    @Test
    public void constantness() throws Exception
    {
        assertTrue( FirstClassObject.isConstant( i1 ) );
    }

    @Test
    public void toJava() throws Exception
    {
        ScreamBaseTest.toJava_(
                SchemeInteger.class,
                Long.class,
                0L,
                SchemeInteger::createObject );
    }

    @Test
    public void basic() throws Exception
    {
        var d = i( 0 );
        assertNotNull( d );
        assertInstanceOf( SchemeInteger.class, d );
        var j = d.toJava();
        assertNotNull( j );
        assertInstanceOf( Long.class, j );

        var d2 = i( d.asLong() );

        assertTrue( d == d2 );
    }

    @Test
    public void equalsTest() throws Exception
    {
        final var i1 = i( 313 );

        {
            var i2 = i( 313 );
            assertEquals( i1, i2 );
            assertEquals( i2, i1 );
        }
        {
            var i2 = i( -313 );
            assertNotEquals( i1, i2 );
            assertNotEquals( i2, i1 );
        }
        {
            var i2 = Cons.NIL;
            assertNotEquals( i1, i2 );
        }
        {
            var i2 = ScreamBaseTest.s313;
            assertNotEquals( i1, i2 );
            assertNotEquals( i2, i1 );
        }
    }

    @Test
    public void equalTest() throws Exception
    {
        {
            var i1 = i( 8 );
            var i2 = i( 8 );

            assertTrue( equal( i1, i2 ) );
        }
        {
            var i1 = i( 313 );
            var i2 = i( 313 );

            assertTrue( equal( i1, i2 ) );
        }
    }

    public interface FuncX<T,R,X extends Exception>
    {
        R apply( T t )
            throws X;
    }

    private void typeFailureTest( FuncX<FirstClassObject, Number, Exception> op ) throws RuntimeX
    {
        var symbol = Symbol.createObject( "313" );
        try
        {
            op.apply( symbol );
            fail();
        }
        catch ( Exception e )
        {
            assertInstanceOf( ScreamException.class, e );
            ScreamException se = (ScreamException)e;
            assertEquals( Code.TYPE_ERROR, se.getCode() );
        }
        try
        {
            op.apply( Cons.NIL );
            fail();
        }
        catch ( Exception e )
        {
            assertInstanceOf( ScreamException.class, e );
            ScreamException se = (ScreamException)e;
            assertEquals( Code.TYPE_ERROR, se.getCode() );
        }
    }

    @Test
    public void add() throws Exception
    {
        var one = i( 1 );
        var two = i( 2 );
        var three = i( 3 );

        var sum = one.add( two );
        assertTrue( equal( three, sum ) );

        typeFailureTest( one::add );
    }

    @Test
    public void subtract() throws Exception
    {
        var one = i( 1 );
        var two = i( 2 );
        var three = i( 3 );

        var v = three.subtract( two );
        assertTrue( equal( one, v ) );

        typeFailureTest( one::subtract );
    }

    @Test
    public void multiply() throws Exception
    {
        var two = ScreamBaseTest.i2;
        var three = ScreamBaseTest.i3;

        var v = three.multiply( two );
        assertTrue( equal( i(6), v ) );

        typeFailureTest( two::multiply );
    }

    @Test
    public void divide() throws Exception
    {
        var fourtynine = i( 21 );
        var seven = i( 7 );
        var div = ScreamBaseTest.i3;

        var v = fourtynine.divide( seven );
        assertTrue( equal( div, v ) );

        typeFailureTest( fourtynine::divide );
    }

    @Test
    public void divisionByZero()
    {
        try
        {
            scriptEngine().evalFco(
                    """
                            (/ 313 0)
                            """ );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.DIVISION_BY_ZERO, rx.getCode() );
            assertNotNull( rx.getMessage() );
        }
    }

}
