package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamException.Code;

public class ScreamExceptionTest
{
    @Test
    public void basic() throws Exception
    {
        ScreamException se = new ScreamException( ScreamException.Code.INTERNAL_ERROR );

        assertEquals( -1, se.getId() );
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
