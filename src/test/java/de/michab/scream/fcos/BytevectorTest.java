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
    @Test
    public void createEmpty() throws Exception
    {
        Bytevector bv = new Bytevector( 5 );

        assertEquals( 0, bv.get( 0 ) );
        assertEquals( 0, bv.get( 1 ) );
        assertEquals( 0, bv.get( 2 ) );
        assertEquals( 0, bv.get( 3 ) );
        assertEquals( 0, bv.get( 4 ) );
    }

    @Test
    public void size() throws Exception
    {
        byte[] value = new byte[]{ (byte)1, (byte)2, (byte)3, (byte)4, (byte)5  };

        Bytevector bv = new Bytevector( value );

        assertEquals( value.length, bv.size() );
    }

    @Test
    public void equals_() throws Exception
    {
        byte[] value = new byte[]{ (byte)1, (byte)2, (byte)3, (byte)4, (byte)5  };

        Bytevector bv1 = new Bytevector( value );
        Bytevector bv2 = new Bytevector( value );

        assertEquals( bv1, bv2 );
    }

    @Test
    public void eqq() throws Exception
    {
        byte[] value = new byte[]{ (byte)1, (byte)2, (byte)3, (byte)4, (byte)5  };

        Bytevector bv1 = new Bytevector( value );
        Bytevector bv2 = new Bytevector( value );

        assertFalse( bv1.eq( bv2 ) );
    }

    @Test
    public void equalq() throws Exception
    {
        byte[] value = new byte[]{ (byte)1, (byte)2, (byte)3, (byte)4, (byte)5  };

        Bytevector bv1 = new Bytevector( value );
        Bytevector bv2 = new Bytevector( value );

        assertTrue( bv1.equal( bv2 ) );
    }

    @Test
    public void content() throws Exception
    {
        byte[] value = new byte[]{ (byte)1, (byte)2, (byte)3, (byte)4, (byte)5  };

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
        byte[] value = new byte[]{ (byte)1, (byte)2, (byte)3, (byte)4, (byte)5  };

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
        byte[] value = new byte[]{ (byte)1, (byte)2, (byte)3, (byte)4, (byte)5  };

        Bytevector bv = FirstClassObject.setConstant( new Bytevector( value ) );

        try
        {
            bv.set( 0, 8 );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.CANT_MODIFY_CONSTANT, rx.getCode() );
        }
    }

    @Test
    public void setOutOfBounds() throws Exception
    {
        byte[] value = new byte[]{ (byte)1, (byte)2, (byte)3, (byte)4, (byte)5  };

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
        byte[] value = new byte[]{ (byte)1, (byte)2, (byte)3, (byte)4, (byte)5  };

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
        byte[] value = new byte[]{ (byte)1, (byte)2, (byte)3, (byte)4, (byte)5  };

        Bytevector bv = new Bytevector( value );

        var s = bv.toString();

        assertEquals( "#u8(1 2 3 4 5)", s );
    }

    @Test
    public void toJava() throws Exception
    {
        byte[] value = new byte[]{ (byte)1, (byte)2, (byte)3, (byte)4, (byte)5  };

        Bytevector bv = new Bytevector( value );

        assertArrayEquals( value, (byte[])bv.toJava() );
    }
}
