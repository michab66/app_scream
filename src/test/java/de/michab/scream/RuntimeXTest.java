package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamException.Code;

public class RuntimeXTest
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

    private void validateMessageAndType( RuntimeX x, Code c, int id )
    {
        var msg = x.getMessage();
        assertNotNull( msg );
        assertEquals(
                c,
                x.getCode() );
        assertEquals(
                id,
                x.getId() );
    }

    @Test
    public void _11_typeError_2() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mTypeError( Cons.class, SchemeDouble.class ),
                Code.TYPE_ERROR,
                11 );
    }
    @Test
    public void _11_typeError_3() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mTypeError( Cons.class, SchemeDouble.class, 313 ),
                Code.TYPE_ERROR,
                11 );
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
