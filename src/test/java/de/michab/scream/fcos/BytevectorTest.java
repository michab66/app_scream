/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamException.Code;

public class BytevectorTest extends ScreamBaseTest
{
    /**
     * Converts the passed integers into a byte array.
     * Checks the value range [0..255].
     *
     * @param bytes The integers.
     * @return A newly allocated byte array.
     */
    private byte[] tba( int ... bytes )
    {
        byte[] result = new byte[ bytes.length ];

        int count = 0;
        for ( var c : bytes )
        {
            if ( c < 0 || c > 255 )
                throw new IllegalArgumentException( "Range error:" + c );
            result[count++] = (byte)c;
        }

        return result;
    }

    @Test
    public void createEmpty() throws Exception
    {
        Bytevector bv = new Bytevector( 0 );

        assertEquals( 0, bv.size() );
    }

    @Test
    public void createUninitialized() throws Exception
    {
        Bytevector bv = new Bytevector( 5 );

        assertEquals( 0, bv.get( 0 ) );
        assertEquals( 0, bv.get( 1 ) );
        assertEquals( 0, bv.get( 2 ) );
        assertEquals( 0, bv.get( 3 ) );
        assertEquals( 0, bv.get( 4 ) );
    }

    @Test
    public void createInitialized() throws Exception
    {
        Bytevector bv = new Bytevector( 5, 13 );

        assertEquals( 13, bv.get( 0 ) );
        assertEquals( 13, bv.get( 1 ) );
        assertEquals( 13, bv.get( 2 ) );
        assertEquals( 13, bv.get( 3 ) );
        assertEquals( 13, bv.get( 4 ) );
    }

    @Test
    public void createFromCons() throws Exception
    {
        Bytevector bv = new Bytevector( (Cons)parse( "(1 2 3)" ) );

        assertEquals( 1, bv.get( 0 ) );
        assertEquals( 2, bv.get( 1 ) );
        assertEquals( 3, bv.get( 2 ) );
        assertFalse( bv.isConstant() );
    }

    @Test
    public void createFromByteArray() throws Exception
    {
        var value = tba( 0, 255 );

        Bytevector bv = new Bytevector( value );

        assertEquals( value.length, bv.size() );
        assertEquals( 0, bv.get( 0 ) );
        assertEquals( 255, bv.get( 1 ) );
    }

    @Test
    public void create_err_RangeExceeded() throws Exception
    {
        try
        {
            new Bytevector( Integer.MAX_VALUE +1 );
            fail();
        }
        catch (RuntimeX rx) {
            assertEquals( Code.RANGE_EXCEEDED, rx.getCode() );
        }
    }

    @Test
    public void size() throws Exception
    {
        byte[] value = tba( 1, 2, 3, 4, 5 );

        Bytevector bv = new Bytevector( value );

        assertEquals( value.length, bv.size() );
    }

    @Test
    public void equals_() throws Exception
    {
        byte[] value = tba( 1, 2, 3, 4, 5 );

        Bytevector bv1 = new Bytevector( value );
        Bytevector bv2 = new Bytevector( value );

        assertEquals( bv1, bv2 );
    }

    @Test
    public void equalq_2() throws Exception
    {
        byte[] value = tba( 1, 2, 3, 4, 5 );

        Bytevector bv1 = new Bytevector( value );

        assertFalse( bv1.equal( Cons.NIL ) );
    }
    @Test
    public void equalq_3() throws Exception
    {
        byte[] value = tba( 1, 2, 3, 4, 5 );

        Bytevector bv1 = new Bytevector( value );

        assertFalse( bv1.equal( i313 ) );
    }

    @Test
    public void eqq() throws Exception
    {
        byte[] value = tba( 1, 2, 3, 4, 5 );

        Bytevector bv1 = new Bytevector( value );
        Bytevector bv2 = new Bytevector( value );

        assertFalse( bv1.eq( bv2 ) );
    }

    @Test
    public void equalq() throws Exception
    {
        byte[] value = tba( 1, 2, 3, 4, 5 );

        Bytevector bv1 = new Bytevector( value );
        Bytevector bv2 = new Bytevector( value );

        assertTrue( bv1.equal( bv2 ) );
    }

    @Test
    public void content() throws Exception
    {
        byte[] value = tba( 1, 2, 3, 4, 5 );

        Bytevector bv = new Bytevector( value );

        assertEquals( 1, bv.get( 0 ) );
        assertEquals( 2, bv.get( 1 ) );
        assertEquals( 3, bv.get( 2 ) );
        assertEquals( 4, bv.get( 3 ) );
        assertEquals( 5, bv.get( 4 ) );
    }

    @Test
    public void set() throws Exception
    {
        byte[] value = tba( 1, 2, 3, 4, 5 );

        Bytevector bv = new Bytevector( value );

        assertEquals( 1, bv.get( 0 ) );
        assertEquals( 2, bv.get( 1 ) );
        assertEquals( 3, bv.get( 2 ) );
        assertEquals( 4, bv.get( 3 ) );
        assertEquals( 5, bv.get( 4 ) );

        bv.set( 0, 8 );
        assertEquals( 8, bv.get( 0 ) );
    }

    @Test
    public void setConstant() throws Exception
    {
        byte[] value = tba( 1, 2, 3, 4, 5 );

        Bytevector bv = FirstClassObject.setConstant( new Bytevector( value ) );

        try
        {
            bv.set( 0, 8 );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.CANNOT_MODIFY_CONSTANT, rx.getCode() );
        }
    }

    @Test
    public void setOutOfBounds() throws Exception
    {
        byte[] value = tba( 1, 2, 3, 4, 5 );

        Bytevector bv = new Bytevector( value );

        try
        {
            bv.set( 313, 8 );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.INDEX_OUT_OF_BOUNDS, rx.getCode() );
        }
    }

    @Test
    public void getOutOfBounds() throws Exception
    {
        byte[] value = tba( 1, 2, 3, 4, 5 );

        Bytevector bv = new Bytevector( value );

        try
        {
            bv.get( 313 );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.INDEX_OUT_OF_BOUNDS, rx.getCode() );
        }
    }

    @Test
    public void tostring() throws Exception
    {
        byte[] value = tba(
                0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                10, 11, 12, 13, 14, 15, 255 );

        Bytevector bv = new Bytevector( value );

        var s = bv.toString();

        assertEquals(
                "#u8(#x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 " +
                    "#x09 #x0a #x0b #x0c #x0d #x0e #x0f #xff)", s );
    }

    @Test
    public void toJava() throws Exception
    {
        byte[] value = tba( 1, 2, 3, 4, 5 );

        Bytevector bv = new Bytevector( value );

        assertArrayEquals( value, (byte[])bv.toJava() );
    }

    @Test
    public void copy() throws Exception
    {
        byte[] value = tba( 1, 2, 3, 4, 5 );

        Bytevector bv1 = new Bytevector( value );
        Bytevector bv2 = bv1.copy();

        assertFalse( bv1 == bv2 );
        assertEquals( bv1.size(), bv2.size() );

        assertEquals( 1, bv1.get( 0 ) );
        assertEquals( 1, bv2.get( 0 ) );
        bv2.set( 0, 0xff );
        assertEquals( 1, bv1.get( 0 ) );
        assertEquals( 0xff, bv2.get( 0 ) );
    }

    @Test
    public void copyStartEnd() throws Exception
    {
        byte[] value = tba( 1, 2, 3, 4, 5 );

        Bytevector bv1 = new Bytevector( value );
        Bytevector bv2 = bv1.copy( 2, 4 );

        assertEquals( 2, bv2.size() );
        assertEquals( 3, bv2.get( 0 ) );
        assertEquals( 4, bv2.get( 1 ) );
    }

    @Test
    public void copyStartEnd_empty() throws Exception
    {
        byte[] value = tba( 1, 2, 3, 4, 5 );

        Bytevector bv1 = new Bytevector( value );
        Bytevector bv2 = bv1.copy( 2, 2 );

        assertEquals( 0, bv2.size() );
    }

    @Test
    public void copyFrom_atFromStartEnd() throws Exception
    {
        Bytevector from =
                new Bytevector( tba( 1, 2, 3, 4, 5 ) );
        Bytevector into =
                new Bytevector( tba( 10, 20, 30, 40, 50 ) );
        into.copyFrom( 1, from, 0, 2 );

        assertEquals( 10, into.get( 0 ) );
        assertEquals( 1, into.get( 1 ) );
        assertEquals( 2, into.get( 2 ) );
        assertEquals( 40, into.get( 3 ) );
        assertEquals( 50, into.get( 4 ) );
    }

    @Test
    public void append() throws Exception
    {
        Bytevector from =
                new Bytevector( tba( 1, 2, 3, 4, 5 ) );
        Bytevector into =
                new Bytevector( tba( 6, 7, 8, 9, 10 ) );
        Bytevector appended = from.append( into );

        assertEquals( from.size() + into.size(), appended.size() );
        assertEquals( 1, appended.get( 0 ) );
        assertEquals( 2, appended.get( 1 ) );
        assertEquals( 3, appended.get( 2 ) );
        assertEquals( 4, appended.get( 3 ) );
        assertEquals( 5, appended.get( 4 ) );
        assertEquals( 6, appended.get( 5 ) );
        assertEquals( 7, appended.get( 6 ) );
        assertEquals( 8, appended.get( 7 ) );
        assertEquals( 9, appended.get( 8 ) );
        assertEquals( 10, appended.get( 9 ) );
    }

}
