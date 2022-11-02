package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

public class RuntimeXTest extends ScreamBaseTest
{
    @Test
    public void internalError() throws Exception
    {
        var se = new RuntimeX( ScreamException.Code.INTERNAL_ERROR.name() );

        assertEquals(
                -1,
                se.getId() );
        assertEquals(
                ScreamException.Code.INTERNAL_ERROR,
                se.getCode() );
    }

    @Test
    public void _16_badBinding() throws Exception
    {
        var se = new RuntimeX( ScreamException.Code.BAD_BINDING.name(), "a1", "a2" );

        assertEquals(
                ScreamException.Code.BAD_BINDING.id(),
                se.getId() );
        assertEquals(
                ScreamException.Code.BAD_BINDING,
                se.getCode() );

        assertTrue( se.getMessage().contains( "a1" ) );
        assertTrue( se.getMessage().contains( "a2" ) );
    }

    @Test
    public void unknownCode() throws Exception
    {
        try
        {
            new RuntimeX( "duck" );
            fail();
        }
        catch ( IllegalArgumentException e )
        {
            assertTrue( e.getMessage().contains( "duck" ) );
        }
    }


//    @Test
//    public void unknownName() throws Exception
//    {
//        try
//        {
//            new ScreamException( ".UNKNOWN" );
//            fail();
//        }
//        catch ( RuntimeException e )
//        {
//            assertEquals( e.getMessage(), "Unknown ScreamException name='.UNKNOWN'" );
//        }
//    }
//
//    @Test
//    public void nameInternal() throws Exception
//    {
//        var se = new ScreamException( "INTERNAL_ERROR" );
//        assertEquals( -1, se.getId() );
//        assertEquals( Code.INTERNAL_ERROR, se.getCode() );
//    }
}
