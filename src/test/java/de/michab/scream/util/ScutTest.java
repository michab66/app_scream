/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2022 Michael G. Binz
 */
package de.michab.scream.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.HashSet;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.SchemeInteger;
import de.michab.scream.fcos.Symbol;

public class ScutTest extends ScreamBaseTest
{
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
        catch ( RuntimeX e )
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
                    (s) -> { throw RuntimeX.mDuplicateElement( s ); } );
            fail();
        }
        catch ( RuntimeX e )
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
    public void asIntegerNotNil() throws Exception
    {
        FirstClassObject fco = i313;

        SchemeInteger i = Scut.asNotNil( SchemeInteger.class, fco );
        assertEquals( 313L, i.asLong() );
    }

    @Test
    public void asIntegerNotNilNil() throws Exception
    {
        try
        {
            FirstClassObject fco = Cons.NIL;

            Scut.asNotNil( SchemeInteger.class, fco );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.TYPE_ERROR, rx.getCode() );
            assertEquals(
                    Cons.NIL,
                    rx.getArgument(1) );
        }
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
            assertEquals( i1, x.getArguments()[0] );
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
            assertEquals( i1, x.getArguments()[0] );
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
            assertEquals( i1, x.getArguments()[0] );
        }
    }
}
