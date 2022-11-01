package de.michab.scream.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

import de.michab.scream.Cons;
import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamException.Code;

public class ScutTest extends ScreamBaseTest
{
    @Test
    public void unique() throws Exception
    {
        var list = readSingleExpression( "(1 2 3)", Cons.class );

        Scut.checkUnique( list );
    }
    @Test
    public void notUnique() throws Exception
    {
        var list = readSingleExpression( "(1 2 3 1)", Cons.class );

        try
        {
            Scut.checkUnique( list );
            fail();
        }
        catch ( RuntimeX x )
        {
            assertEquals( Code.DUPLICATE_ELEMENT, x.getCode() );
            assertEquals( i1.toString(), x.getArguments()[0] );
        }
    }
    @Test
    public void uniqueNotProper() throws Exception
    {
        var list = readSingleExpression( "(1 2 3 . 4)", Cons.class );

        Scut.checkUnique( list );
    }
    @Test
    public void notUniqueNotProper() throws Exception
    {
        var list = readSingleExpression( "(1 2 3 . 1)", Cons.class );

        try
        {
            Scut.checkUnique( list );
            fail();
        }
        catch ( RuntimeX x )
        {
            assertEquals( Code.DUPLICATE_ELEMENT, x.getCode() );
            assertEquals( i1.toString(), x.getArguments()[0] );
        }
    }
}
