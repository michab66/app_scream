package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamException.Code;

public class SchemeDoubleTest
{
    @Test
    public void basic() throws Exception
    {
        var d = SchemeDouble.createObject( 0.0 );
        assertNotNull( d );
        assertInstanceOf( SchemeDouble.class, d );
        var j = d.toJava();
        assertNotNull( j );
        assertInstanceOf( Double.class, j );

        var d2 = SchemeDouble.createObject( d.asDouble() );

        assertTrue( d == d2 );
    }

    @Test
    public void basic2() throws Exception
    {
        for ( double i = 0 ; i < 100 ; i += .5 )
        {
            var d = SchemeDouble.createObject( i );
            assertNotNull( d );
            assertInstanceOf( SchemeDouble.class, d );
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
        var one = SchemeDouble.createObject( 1.0 );
        var two = SchemeDouble.createObject( 2.0 );
        var three = SchemeDouble.createObject( 3.0 );

        var sum = one.add( two );
        assertEquals( three, sum );

        typeFailureTest( one::add );
    }

    @Test
    public void subtract() throws Exception
    {
        var one = SchemeDouble.createObject( 1.0 );
        var two = SchemeDouble.createObject( 2.0 );
        var three = SchemeDouble.createObject( 3.0 );

        var v = three.subtract( two );
        assertEquals( one, v );

        typeFailureTest( one::subtract );
    }

    @Test
    public void multiply() throws Exception
    {
        var two = SchemeDouble.createObject( 2.0 );
        var three = SchemeDouble.createObject( 3.0 );

        var v = three.multiply( two );
        assertEquals( SchemeDouble.createObject( 6.0 ), v );

        typeFailureTest( two::multiply );
    }

    @Test
    public void divide() throws Exception
    {
        var two = SchemeDouble.createObject( 2.0 );
        var three = SchemeDouble.createObject( 3.0 );
        var div = SchemeDouble.createObject( 1.5 );

        var v = three.divide( two );
        assertEquals( div, v );

        typeFailureTest( two::divide );
    }
}
