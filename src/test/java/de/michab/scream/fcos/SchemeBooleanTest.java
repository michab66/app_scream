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

public class SchemeBooleanTest extends ScreamBaseTest
{
    @Test
    public void toJava() throws Exception
    {
        ScreamBaseTest.toJava_(
                SchemeBoolean.class,
                Boolean.class,
                true,
                SchemeBoolean::createObject );
    }

    @Test
    public void isTrue() throws Exception
    {
        assertFalse( SchemeBoolean.isTrue( SchemeBoolean.F ) );
        assertTrue( SchemeBoolean.isTrue( Cons.NIL ) );
        assertTrue( SchemeBoolean.isTrue( i313 ) );
        assertTrue( SchemeBoolean.isTrue( s313 ) );
        assertTrue( SchemeBoolean.isTrue( c(i1,i2) ) );
        assertTrue( SchemeBoolean.isTrue( SchemeBoolean.T ) );
    }
}
