package de.michab.scream.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.HashSet;

import org.junit.jupiter.api.Test;

import de.michab.scream.Cons;
import de.michab.scream.FirstClassObject;
import de.michab.scream.RuntimeX;
import de.michab.scream.SchemeDouble;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamException.Code;

public class ScutTest extends ScreamBaseTest
{
    private RuntimeX validateMessageAndType( RuntimeX x, Code c, int id )
    {
        var msg = x.getMessage();
        assertNotNull( msg );
        assertEquals(
                c,
                x.getCode() );
        assertEquals(
                id,
                x.getId() );
        return x;
    }


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

    @Test
    public void notUniqueMultiStep() throws Exception
    {
        HashSet<FirstClassObject> unifier = new HashSet<>();

        var l1 = readSingleExpression( "(1 2 3)", Cons.class );
        var l2 = readSingleExpression( "(4 5 1)", Cons.class );

        try
        {
            Scut.checkUnique( unifier, l1 );
            Scut.checkUnique( unifier, l2 );
            fail();
        }
        catch ( RuntimeX x )
        {
            assertEquals( Code.DUPLICATE_ELEMENT, x.getCode() );
            assertEquals( i1.toString(), x.getArguments()[0] );
        }
    }

    @Test
    public void _11_typeError_2() throws Exception
    {
        validateMessageAndType(
                Scut.mTypeError( Cons.class, SchemeDouble.class ),
                Code.TYPE_ERROR,
                11 );
    }
    @Test
    public void _11_typeError_3() throws Exception
    {
        validateMessageAndType(
                Scut.mTypeError( Cons.class, SchemeDouble.class, 313 ),
                Code.TYPE_ERROR,
                11 );
    }

    @Test
    public void _46_duplicateElement_1() throws Exception
    {
        var x = validateMessageAndType(
                Scut.mDuplicateElement( s3 ),
                Code.DUPLICATE_ELEMENT,
                46 );
        assertNotNull( x.getArguments() );
        assertEquals( 1, x.getArguments().length );
        assertEquals( s3.toString(), x.getArguments()[0] );
    }
}
