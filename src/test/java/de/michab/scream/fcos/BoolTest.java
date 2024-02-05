/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;

public class BoolTest extends ScreamBaseTest
{
    @Test
    public void toJava() throws Exception
    {
        ScreamBaseTest.toJava_(
                Bool.class,
                Boolean.class,
                true,
                Bool::createObject );
    }

    @Test
    public void isTrue() throws Exception
    {
        assertFalse( Bool.isTrue( Bool.F ) );
        assertTrue( Bool.isTrue( Cons.NIL ) );
        assertTrue( Bool.isTrue( i313 ) );
        assertTrue( Bool.isTrue( s313 ) );
        assertTrue( Bool.isTrue( c(i1,i2) ) );
        assertTrue( Bool.isTrue( Bool.T ) );
    }
}
