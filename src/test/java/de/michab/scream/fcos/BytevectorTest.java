/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;

public class BytevectorTest extends ScreamBaseTest
{
    @Test
    public void size() throws Exception
    {
        byte[] value = new byte[]{ (byte)1, (byte)2, (byte)3, (byte)4, (byte)5  };

        Bytevector bv = new Bytevector( value );

        assertEquals( value.length, bv.size() );
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
    public void tostring() throws Exception
    {
        byte[] value = new byte[]{ (byte)1, (byte)2, (byte)3, (byte)4, (byte)5  };

        Bytevector bv = new Bytevector( value );

        var s = bv.toString();

        assertEquals( "#u8(1 2 3 4 5)", s );
    }

    @Test
    public void tosJava() throws Exception
    {
        byte[] value = new byte[]{ (byte)1, (byte)2, (byte)3, (byte)4, (byte)5  };

        Bytevector bv = new Bytevector( value );

        assertArrayEquals( value, (byte[])bv.toJava() );
    }
}
