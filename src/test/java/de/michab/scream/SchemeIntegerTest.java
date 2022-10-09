package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamException.Code;

public class SchemeIntegerTest
{
    private SchemeInteger mk( long i )
    {
        return SchemeInteger.createObject( i );
    }

    private boolean equal( FirstClassObject a, FirstClassObject b )
    {
        var r1 = FirstClassObject.equal( a, b );
        var r2 = FirstClassObject.equal( b, a );
        assertEquals( r1, r2 );
        return r1;
    }

    @Test
    public void basic() throws Exception
    {
        var d = mk( 0 );
        assertNotNull( d );
        assertInstanceOf( SchemeInteger.class, d );
        var j = d.convertToJava();
        assertNotNull( j );
        assertInstanceOf( Long.class, j );

        var d2 = mk( d.asLong() );

        assertTrue( d == d2 );
    }

    @Test
    public void equalsTest() throws Exception
    {
        final var i1 = mk( 313 );

        {
            var i2 = mk( 313 );
            assertEquals( i1, i2 );
            assertEquals( i2, i1 );
        }
        {
            var i2 = mk( -313 );
            assertNotEquals( i1, i2 );
            assertNotEquals( i2, i1 );
        }
        {
            var i2 = Cons.NIL;
            assertNotEquals( i1, i2 );
        }
        {
            var i2 = TestUtil.s313;
            assertNotEquals( i1, i2 );
            assertNotEquals( i2, i1 );
        }
    }

    @Test
    public void equalTest() throws Exception
    {
        {
            var i1 = mk( 8 );
            var i2 = mk( 8 );

            assertTrue( equal( i1, i2 ) );
        }
        {
            var i1 = mk( 313 );
            var i2 = mk( 313 );

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
        var one = mk( 1 );
        var two = mk( 2 );
        var three = mk( 3 );

        var sum = one.add( two );
        assertTrue( equal( three, sum ) );

        typeFailureTest( one::add );
    }

    @Test
    public void subtract() throws Exception
    {
        var one = mk( 1 );
        var two = mk( 2 );
        var three = mk( 3 );

        var v = three.subtract( two );
        assertTrue( equal( one, v ) );

        typeFailureTest( one::subtract );
    }

    @Test
    public void multiply() throws Exception
    {
        var two = TestUtil.i2;
        var three = TestUtil.i3;

        var v = three.multiply( two );
        assertTrue( equal( mk(6), v ) );

        typeFailureTest( two::multiply );
    }

    @Test
    public void divide() throws Exception
    {
        var fourtynine = mk( 21 );
        var seven = mk( 7 );
        var div = TestUtil.i3;

        var v = fourtynine.divide( seven );
        assertTrue( equal( div, v ) );

        typeFailureTest( fourtynine::divide );
    }
}
