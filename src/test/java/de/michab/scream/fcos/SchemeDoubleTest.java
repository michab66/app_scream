/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;

public class SchemeDoubleTest extends ScreamBaseTest
{
    private Double ZERO = 0.0d;

    @Test
    public void constantness() throws Exception
    {
        assertTrue(
                FirstClassObject.isConstant(
                        SchemeDouble.createObject( ZERO ) ) );
    }

    @Test
    public void toJava() throws Exception
    {
        ScreamBaseTest.toJava_(
                SchemeDouble.class,
                Double.class,
                ZERO,
                SchemeDouble::createObject );
    }

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

        assertEqualq( d, d2 );
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

    /**
     * Checks if the operation can be applied on a symbol.
     * @param op
     * @throws Exception fails it can be applied to a symbol.
     */
    private void typeFailureTest( FuncX<FirstClassObject, Number, Exception> op ) throws Exception
    {
        var symbol = Symbol.createObject( "313" );
        try
        {
            op.apply( symbol );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.TYPE_ERROR, rx.getCode() );
        }
        try
        {
            op.apply( Cons.NIL );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.TYPE_ERROR, rx.getCode() );
        }
    }

    @Test
    public void add() throws Exception
    {
        var one = SchemeDouble.createObject( 1.0 );
        var two = SchemeDouble.createObject( 2.0 );
        var three = SchemeDouble.createObject( 3.0 );

        var sum = one.add( two );
        assertEqualq( three, sum );

        typeFailureTest( one::add );
    }

    @Test
    public void subtract() throws Exception
    {
        var one = SchemeDouble.createObject( 1.0 );
        var two = SchemeDouble.createObject( 2.0 );
        var three = SchemeDouble.createObject( 3.0 );

        assertEqualq(
                one,
                three.subtract( two ) );

        typeFailureTest( one::subtract );
    }

    @Test
    public void multiply() throws Exception
    {
        var two = SchemeDouble.createObject( 2.0 );
        var three = SchemeDouble.createObject( 3.0 );

        var v = three.multiply( two );
        assertEqualq( SchemeDouble.createObject( 6.0 ), v );

        typeFailureTest( two::multiply );
    }

    @Test
    public void divide() throws Exception
    {
        var two = SchemeDouble.createObject( 2.0 );
        var three = SchemeDouble.createObject( 3.0 );
        var div = SchemeDouble.createObject( 1.5 );

        var v = three.divide( two );
        assertEqualq( div, v );

        typeFailureTest( two::divide );
    }
}
