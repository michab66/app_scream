/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 2022 Michael G. Binz
 */
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
import de.michab.scream.SchemeInteger;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamException;
import de.michab.scream.ScreamException.Code;
import de.michab.scream.Symbol;

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
    public void asInteger() throws Exception
    {
        FirstClassObject fco = i313;

        SchemeInteger i = Scut.as( SchemeInteger.class, fco );
        assertEquals( 313L, i.asLong() );
    }

    @Test
    public void asIntegerNil() throws Exception
    {
        FirstClassObject fco = Cons.NIL;

        SchemeInteger i = Scut.as( SchemeInteger.class, fco );
        assertEquals( Cons.NIL, i );
    }

    @Test
    public void asSymbolNil() throws Exception
    {
        FirstClassObject fco = Cons.NIL;

        Symbol i = Scut.as( Symbol.class, fco );
        assertEquals( Cons.NIL, i );
    }

    @Test
    public void asIntegerFail() throws Exception
    {
        FirstClassObject fco = s313;

        try
        {
            Scut.as( SchemeInteger.class, fco );
            fail();
        }
        catch ( ScreamException e )
        {
            assertEquals( Code.TYPE_ERROR, e.getCode() );
            // Check for format error.
            assertNotNull( e.getMessage() );
        }
    }

    @Test
    public void asIntegerFail2() throws Exception
    {
        FirstClassObject fco = s313;

        try
        {
            Scut.as( SchemeInteger.class,
                    fco,
                    (s) -> { throw Scut.mDuplicateElement( s ); } );
            fail();
        }
        catch ( ScreamException e )
        {
            assertEquals( Code.DUPLICATE_ELEMENT, e.getCode() );
            // Check for format error.
            assertNotNull( e.getMessage() );
        }
    }

    @Test
    public void asIntegerFailAndChange() throws Exception
    {
        FirstClassObject fco = s313;

        SchemeInteger i = Scut.as(
                SchemeInteger.class,
                fco,
                (s) -> { return i2; } );
        assertEquals( 2L, i.asLong() );
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
    public void _17_badClause_3() throws Exception
    {
        var badClause = parse( "(bad clause)" );

        var x = validateMessageAndType(
                Scut.mBadClause( badClause ),
                Code.BAD_CLAUSE,
                17 );
        assertEquals( badClause.toString(), x.getArguments()[0] );
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
