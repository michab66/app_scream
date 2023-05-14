/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;

public class EnvironmentTest extends ScreamBaseTest
{
    @Test
    public void get() throws Exception
    {
        var environment = new Environment( "test-environment" ).define( s313, i313 );
        assertEquals( i313, environment.get( s313 ) );
    }

    @Test
    public void unset() throws Exception
    {
        var environment = new Environment( "test-environment" ).define( s313, i313 );
        assertEquals( i313, environment.get( s313 ) );
        environment.unset( s313 );
        try
        {
            environment.get( s313 );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.SYMBOL_NOT_DEFINED, rx.getCode() );
        }
    }

    @Test
    public void assignFail() throws Exception
    {
        var environment = new Environment( "test-environment" );

        try
        {
            environment.assign( s313, i1 );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.SYMBOL_NOT_ASSIGNABLE, rx.getCode() );
        }
    }

    @Test
    public void getFail() throws Exception
    {
        var environment = new Environment( "test-environment" );

        try
        {
            environment.get( s1 );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.SYMBOL_NOT_DEFINED, rx.getCode() );
        }
    }

    @Test
    public void constantness() throws Exception
    {
        var environment = new Environment( "test-environment" );

        assertFalse( environment.isConstant() );
        environment.setConstant();
        assertTrue( environment.isConstant() );
    }

    @Test
    public void toStringTest()
    {
        var e0 = new Environment( "test-environment" );
        assertEquals(
                String.format( "<Environment:/test-environment(%d)>", e0.id() ),
                e0.toString() );
        var e1 = e0.extend( "1" );
        assertEquals(
                String.format( "<Environment:/test-environment(%d)/1(%d)>",
                        e0.id(),
                        e1.id() ),
                e1.toString() );
        var e2 = e1.extend( "2" );
        assertEquals(
                String.format( "<Environment:/test-environment(%d)/1(%d)/2(%d)>",
                        e0.id(),
                        e1.id(),
                        e2.id() ),
                e2.toString() );
        var e3 = e2.extend( "3" );
        assertEquals(
                String.format( "<Environment:/test-environment(%d)/1(%d)/2(%d)/3(%d)>",
                        e0.id(),
                        e1.id(),
                        e2.id(),
                        e3.id() ),
                e3.toString() );
        var e4 = e3.extend( "4" );
        assertEquals(
                String.format( "<Environment:/test-environment(%d)/1(%d)/2(%d)/3(%d)/4(%d)>",
                        e0.id(),
                        e1.id(),
                        e2.id(),
                        e3.id(),
                        e4.id() ),
                e4.toString() );
        var e5 = e4.extend( "5" );
        assertEquals(
                String.format( "<Environment:/test-environment(%d)/1(%d)/2(%d)/3(%d)/4(%d)/5(%d)>",
                        e0.id(),
                        e1.id(),
                        e2.id(),
                        e3.id(),
                        e4.id(),
                        e5.id() ),
                e5.toString() );
    }
}

