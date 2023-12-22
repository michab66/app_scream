/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2022-2023 Michael G. Binz
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
import de.michab.scream.fcos.Number;
import de.michab.scream.fcos.SchemeDouble;
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
                    FirstClassObject.toString( Cons.NIL ),
                    rx.getArgument(1) );
        }
    }

    @Test
    public void unique_numbers() throws Exception
    {
        var list = parse( "(1 2 3)", Cons.class );

        Scut.assertUnique( list );
    }

    @Test
    public void unique_symbols() throws Exception
    {
        var list = parse( "(a b c)", Cons.class );

        Scut.assertUnique( list );
    }

    @Test
    public void unique_symbols_improper() throws Exception
    {
        var list = parse( "(a b c . rest)", Cons.class );

        Scut.assertUnique( list );
    }

    @Test
    public void unique_mixed() throws Exception
    {
        var list = parse( "(a 1 b 2)", Cons.class );

        Scut.assertUnique( list );
    }

    @Test
    public void notUnique_numbers() throws Exception
    {
        var list = parse( "(1 2 3 1)", Cons.class );

        try
        {
            Scut.assertUnique( list );
            fail();
        }
        catch ( RuntimeX x )
        {
            assertEquals( Code.DUPLICATE_ELEMENT, x.getCode() );
            assertEquals( i1, x.getArguments()[0] );
        }
    }

    @Test
    public void notUnique_symbols() throws Exception
    {
        var list = parse( "(a b b a)", Cons.class );

        try
        {
            Scut.assertUnique( list );
            fail();
        }
        catch ( RuntimeX x )
        {
            assertEquals( Code.DUPLICATE_ELEMENT, x.getCode() );
            assertEquals( s("b"), x.getArguments()[0] );
        }
    }

    @Test
    public void notUnique_symbols_improper() throws Exception
    {
        var list = parse( "(a b . b)", Cons.class );

        try
        {
            Scut.assertUnique( list );
            fail();
        }
        catch ( RuntimeX x )
        {
            assertEquals( Code.DUPLICATE_ELEMENT, x.getCode() );
            assertEquals( s("b"), x.getArguments()[0] );
        }
    }

    @Test
    public void notUnique_mixed() throws Exception
    {
        var list = parse( "(a 1 b 1)", Cons.class );

        try
        {
            Scut.assertUnique( list );
            fail();
        }
        catch ( RuntimeX x )
        {
            assertEquals( Code.DUPLICATE_ELEMENT, x.getCode() );
            assertEquals( i(1), x.getArguments()[0] );
        }
    }

    @Test
    public void uniqueNotProper() throws Exception
    {
        var list = parse( "(1 2 3 . 4)", Cons.class );

        Scut.assertUnique( list );
    }

    @Test
    public void notUniqueNotProper() throws Exception
    {
        var list = parse( "(1 2 3 . 1)", Cons.class );

        try
        {
            Scut.assertUnique( list );
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

        var l1 = parse( "(1 2 3)", Cons.class );
        var l2 = parse( "(4 5 1)", Cons.class );

        try
        {
            Scut.assertUnique( unifier, l1 );
            Scut.assertUnique( unifier, l2 );
            fail();
        }
        catch ( RuntimeX x )
        {
            assertEquals( Code.DUPLICATE_ELEMENT, x.getCode() );
            assertEquals( i1, x.getArguments()[0] );
        }
    }

    @Test
    public void assertHomogeneous_integers() throws Exception
    {
        var list = parse( "(1 2 3)", Cons.class );

        Scut.assertHomogeneous( list, SchemeInteger.class );
    }
    @Test
    public void assertHomogeneous_floats() throws Exception
    {
        var list = parse( "(1. 2. 3.)", Cons.class );

        Scut.assertHomogeneous( list, SchemeDouble.class );
    }
    @Test
    public void assertHomogeneous_numbers() throws Exception
    {
        var list = parse( "(1 2. 3)", Cons.class );

        Scut.assertHomogeneous( list, Number.class );
    }
    @Test
    public void assertHomogeneous_symbols() throws Exception
    {
        var list = parse( "(tick trick track)", Cons.class );

        Scut.assertHomogeneous( list, Symbol.class );
    }
    @Test
    public void assertHomogeneous_fail_integers() throws Exception
    {
        var list = parse( "(1 2. 3)", Cons.class );

        try
        {
            Scut.assertHomogeneous( list, SchemeInteger.class );
            fail();
        }
        catch ( RuntimeX x )
        {
            assertEquals(
                    Code.TYPE_ERROR,
                    x.getCode() );
            assertEquals(
                    FirstClassObject.typename( SchemeInteger.class ),
                    x.getArguments()[0] );
            assertEquals(
                    FirstClassObject.typename( SchemeDouble.class ) + "=2.0",
                    x.getArguments()[1] );
        }
    }
}
