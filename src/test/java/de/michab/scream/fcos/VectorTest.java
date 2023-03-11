/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamException;
import de.michab.scream.ScreamException.Code;

public class VectorTest extends ScreamBaseTest
{
    @Test
    public void basic() throws Exception
    {
        var size = 5;
        Vector v = new Vector( size );

        assertEquals( size, v.size() );
        for ( int i = 0 ; i < size ; i++ )
            assertEquals( Cons.NIL, v.get( i ) );

        v.fill( ScreamBaseTest.s1 );
        for ( int i = 0 ; i < size ; i++ )
            assertEquals( ScreamBaseTest.s1, v.get( i ) );
    }

    @Test
    public void indexErr() throws Exception
    {
        var size = 5;
        Vector v = new Vector( size );

        try {
            v.get( size );
            fail();
        }
        catch ( ScreamException e ) {
            assertEquals( Code.INDEX_OUT_OF_BOUNDS, e.getCode() );
        }
        try {
            v.set( size, Cons.NIL );
            fail();
        }
        catch ( ScreamException e ) {
            assertEquals( Code.INDEX_OUT_OF_BOUNDS, e.getCode() );
        }
    }

    @Test
    public void copyYes() throws Exception
    {
        var a = new FirstClassObject[]
        {
            i1,
            i2
        };

        var vCopy = new Vector( a );
        assertEquals( a.length, vCopy.size() );
        vCopy.set( 0, s313 );
        assertEquals( s313, vCopy.get( 0 ) );
    }

    @Test
    public void copyNo() throws Exception
    {
        var a = new FirstClassObject[]
        {
            ScreamBaseTest.i1,
            ScreamBaseTest.i2
        };

        var vCopy = new Vector( a, false );
        assertEquals( a.length, vCopy.size() );
        vCopy.set( 0, s313 );
        assertEquals( s313, vCopy.get( 0 ) );
        assertEquals( s313, a[0] );
    }

    @Test
    public void vector_length_Test() throws Exception
    {
        var se = scriptEngine();

        var result = se.evalFco(
                """
                (define v #(1 2 3))
                (vector-length v)
                """ );
        assertEquals( i3, result );
    }

    @Test
    public void vector_ref_Test() throws Exception
    {
        var se = scriptEngine();

        var result = se.evalFco(
                """
                (define v #(1 2 3))
                (vector-ref v 0)
                """ );
        assertEquals( i1, result );
    }

    @Test
    public void vector_set_Test() throws Exception
    {
        var se = scriptEngine();

        var result = se.evalFco(
                """
                (define v #(1 2 3))
                (vector-set! v 0 313)
                (vector-ref v 0)
                """ );
        assertEquals( i313, result );
    }

    @Test
    public void pvector_Test() throws Exception
    {
        var se = scriptEngine();

        var result = se.evalFco(
                """
                (vector? #(1 2 3))
                """ );
        assertEquals( SchemeBoolean.T, result );
    }

    @Test
    public void cannotModifyConst1() throws Exception
    {
        Vector v = (Vector)scriptEngine().evalFco(
                """
                '#(1 2 3)
                """ );
        try
        {
            v.fill( i1 );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.CANT_MODIFY_CONSTANT, rx.getCode() );
        }
    }

    @Test
    public void cannotModifyConst2() throws Exception
    {
        Vector v = (Vector)scriptEngine().evalFco(
                """
                '#(1 2 3)
                """ );
        try
        {
            v.set( 0, i1 );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.CANT_MODIFY_CONSTANT, rx.getCode() );
        }
    }
}
