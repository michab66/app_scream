/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

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
            assertEquals( -1, se.getId() );
            assertEquals( Code.INTERNAL_ERROR, se.getCode() );
        }
        {
            var se = new ScreamException( ScreamException.Code.INTERNAL_ERROR );
            assertEquals( -1, se.getId() );
            assertEquals( Code.INTERNAL_ERROR, se.getCode() );
        }

        {
            var se = new ScreamException( ScreamException.Code.NOT_IMPLEMENTED.toString() );
            assertEquals( 0, se.getId() );
            assertEquals( Code.NOT_IMPLEMENTED, se.getCode() );
        }
        {
            var se = new ScreamException( ScreamException.Code.NOT_IMPLEMENTED );
            assertEquals( 0, se.getId() );
            assertEquals( Code.NOT_IMPLEMENTED, se.getCode() );
        }

        {
            var se = new ScreamException( ScreamException.Code.SYMBOL_NOT_DEFINED.toString(), arg1 );
            assertEquals( 1, se.getId() );
            assertEquals( Code.SYMBOL_NOT_DEFINED, se.getCode() );
            assertTrue( se.getMessage().contains( arg1 ) );
        }
        {
            var se = new ScreamException( ScreamException.Code.SYMBOL_NOT_DEFINED, arg1 );
            assertEquals( 1, se.getId() );
            assertEquals( Code.SYMBOL_NOT_DEFINED, se.getCode() );
            assertTrue( se.getMessage().contains( arg1 ) );
        }

        {
            var se = new ScreamException( ScreamException.Code.SYMBOL_NOT_ASSIGNABLE.toString(), arg1 );
            assertEquals( 2, se.getId() );
            assertEquals( Code.SYMBOL_NOT_ASSIGNABLE, se.getCode() );
            assertTrue( se.getMessage().contains( arg1 ) );
        }
        {
            var se = new ScreamException( ScreamException.Code.SYMBOL_NOT_ASSIGNABLE, arg1 );
            assertEquals( 2, se.getId() );
            assertEquals( Code.SYMBOL_NOT_ASSIGNABLE, se.getCode() );
            assertTrue( se.getMessage().contains( arg1 ) );
        }

    }

    @Test
    public void unknownName() throws Exception
    {
        try
        {
            new ScreamException( ".UNKNOWN" );
            fail();
        }
        catch ( RuntimeException e )
        {
            assertEquals( e.getMessage(), "Unknown ScreamException name='.UNKNOWN'" );
        }
    }

    @Test
    public void nameInternal() throws Exception
    {
        var se = new ScreamException( "INTERNAL_ERROR" );
        assertEquals( -1, se.getId() );
        assertEquals( Code.INTERNAL_ERROR, se.getCode() );
    }
}
