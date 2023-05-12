/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamException.Code;

public class ScreamExceptionTest
{
    @Test
    public void basic() throws Exception
    {
        final var arg1 = "arg1";

        {
            var se = new ScreamException( ScreamException.Code.INTERNAL_ERROR.toString() );
            assertEquals( Code.INTERNAL_ERROR, se.getCode() );
        }
        {
            var se = new ScreamException( ScreamException.Code.INTERNAL_ERROR );
            assertEquals( Code.INTERNAL_ERROR, se.getCode() );
        }

        {
            var se = new ScreamException( ScreamException.Code.NOT_IMPLEMENTED.toString() );
            assertEquals( Code.NOT_IMPLEMENTED, se.getCode() );
        }
        {
            var se = new ScreamException( ScreamException.Code.NOT_IMPLEMENTED );
            assertEquals( Code.NOT_IMPLEMENTED, se.getCode() );
        }

        {
            var se = new ScreamException( ScreamException.Code.SYMBOL_NOT_DEFINED.toString(), arg1 );
            assertEquals( Code.SYMBOL_NOT_DEFINED, se.getCode() );
            assertTrue( se.getMessage().contains( arg1 ) );
        }
        {
            var se = new ScreamException( ScreamException.Code.SYMBOL_NOT_DEFINED, arg1 );
            assertEquals( Code.SYMBOL_NOT_DEFINED, se.getCode() );
            assertTrue( se.getMessage().contains( arg1 ) );
        }

        {
            var se = new ScreamException( ScreamException.Code.SYMBOL_NOT_ASSIGNABLE.toString(), arg1 );
            assertEquals( Code.SYMBOL_NOT_ASSIGNABLE, se.getCode() );
            assertTrue( se.getMessage().contains( arg1 ) );
        }
        {
            var se = new ScreamException( ScreamException.Code.SYMBOL_NOT_ASSIGNABLE, arg1 );
            assertEquals( Code.SYMBOL_NOT_ASSIGNABLE, se.getCode() );
            assertTrue( se.getMessage().contains( arg1 ) );
        }

    }

    @Test
    public void unknownName() throws Exception
    {
            var sex = new ScreamException( ".UNKNOWN", 1, 2, 3 );
            assertEquals( Code.ERROR, sex.getCode() );
            assertEquals( "35 : .UNKNOWN 1 2 3", sex.getMessage() );
    }

    @Test
    public void nameInternal() throws Exception
    {
        var se = new ScreamException( "INTERNAL_ERROR" );
        assertEquals( Code.INTERNAL_ERROR, se.getCode() );
    }
}
