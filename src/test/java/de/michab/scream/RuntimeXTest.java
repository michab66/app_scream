/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.util.ErrorMessages;

public class RuntimeXTest extends ScreamBaseTest
{
    @Test
    public void blankError() throws Exception
    {
        try
        {
            new RuntimeX( str("") );
            fail();
        }
        catch ( IllegalArgumentException expected )
        {
        }
    }

    @Test
    public void paramMismatch() throws Exception
    {
        // BAD_BINDING is only defined by BAD_BINDING_2, this
        // tests the fallback to BAD_BINDING.
        assertFalse(
                ErrorMessages.map.containsKey( Code.BAD_BINDING.toString() )
        );

        var se = new RuntimeX( Code.BAD_BINDING, s( "unknown" ) );

        assertEquals(
                Code.BAD_BINDING,
                se.getCode() );
    }

    @Test
    public void internalError() throws Exception
    {
        var se = new RuntimeX( str( Code.INTERNAL_ERROR.name() ) );

        assertEquals(
                Code.INTERNAL_ERROR,
                se.getCode() );
    }

    @Test
    public void undefinedName() throws Exception
    {
        final var name = "Gobbledigoog";

        var se = new RuntimeX( str( name ) );

        assertEquals(
                Code.ERROR,
                se.getCode() );
        assertNotNull(
                se.getArguments() );
        assertEquals(
                name,
                se.getRawMessage() );
    }

    @Test
    public void undefinedNameAndIrritants() throws Exception
    {
        var se = new RuntimeX(
                str( "Gobbledigoog" ),
                i313,
                i1,
                s313,
                str( "A string." ) );

        assertEquals(
                Code.ERROR,
                se.getCode() );

        var irritants = se.getArguments();

        assertEquals(
                4, irritants.length );
        assertEqualq(
                i313,
                irritants[0] );
        assertEqualq(
                i1,
                irritants[1] );
        assertEqualq(
                s313,
                irritants[2] );
        assertEqualq(
                str("A string."),
                irritants[3] );
    }

    @Test
    public void _badBinding() throws Exception
    {
        var se = new RuntimeX(
                str( Code.BAD_BINDING.name() ),
                str( "a1" ),
                str( "a2" ) );

        assertEquals(
                Code.BAD_BINDING,
                se.getCode() );
        assertEquals(
                Code.BAD_BINDING,
                se.getCode() );
        assertEquals(
                Code.BAD_BINDING.toString(),
                se.getRawMessage() );

        assertTrue( se.getMessage().contains( "a1" ) );
        assertTrue( se.getMessage().contains( "a2" ) );
    }

    @Test
    public void operationName() throws Exception
    {
        var se = Raise.mDivisionByZero().setOperationName( s313 );

        assertEquals(
                Code.DIVISION_BY_ZERO,
                se.getCode() );

        var splitMessage = se.getMessage().split( " : " );
        assertEquals( s313.toString(), splitMessage[1] );
    }

    @Test
    public void unknownCode() throws Exception
    {
        var rx = new RuntimeX( str( "duck" ) );
        assertEquals( Code.ERROR, rx.getCode() );
        assertEquals( Code.ERROR + " : duck", rx.getMessage() );
    }

    @Test
    public void unknownName() throws Exception
    {
            var sex = new RuntimeX(
                    str( ".UNKNOWN" ),
                    i1, i2, i3 );
            assertEquals( Code.ERROR, sex.getCode() );
            assertEquals( Code.ERROR + " : .UNKNOWN 1 2 3", sex.getMessage() );
    }
}
