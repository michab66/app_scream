/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2023 Michael G. Binz
 */
package de.michab.scream.pops;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Environment;
import de.michab.scream.fcos.SchemeInteger;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.util.Scut;


public class PrimitivesTest extends ScreamBaseTest
{
    @Test
    public void _cast() throws Exception
    {
        var result = cont().toStack(
                cont -> Primitives._cast(
                        SchemeInteger.class,
                        i313,
                        // Intermediate continuation of type
                        // Cont<SchemeInteger> required.
                        casted -> cont.accept( casted ) ) );

        assertEqualq( i313, result );
    }

    @Test
    public void _castFail() throws Exception
    {
        RuntimeX rx =
                assertThrows(
                        RuntimeX.class,
                        () -> {
                            cont().toStack(
                                    cont -> Primitives._cast(
                                            Symbol.class,
                                            i313,
                                            null ) );
                        } );

        assertEquals( Code.TYPE_ERROR, rx.getCode() );
    }

    @Test
    public void defineList() throws Exception
    {
        Environment e = new Environment( getClass().getSimpleName() );

        Cons expected = Scut.as( Cons.class, parse( "(a b c)" ) );

        var result = cont().toStack(
                cont -> Primitives._x_defineList(
                        e,
                        expected,
                        Scut.as( Cons.class, parse( "(1 2 3)" ) ),
                        cont ) );

        assertEqualq( expected, result );
    }

    @Test
    public void defineList_lessValues() throws Exception
    {
        Environment e = new Environment( getClass().getSimpleName() );

        Cons expected = Scut.as( Cons.class, parse( "(a b)" ) );

        var result = cont().toStack(
                cont -> Primitives._x_defineList(
                        e,
                        expected,
                        Scut.as( Cons.class, parse( "(1)" ) ),
                        cont ) );

        assertEqualq( expected, result );
        assertEqualq( e.get( s("a") ), i(1) );
        assertEqualq( e.get( s("b") ), Cons.NIL );
    }

    @Test
    public void defineList_moreValues() throws Exception
    {
        Environment e = new Environment( getClass().getSimpleName() );

        Cons expected = Scut.as( Cons.class, parse( "(a b)" ) );

        var result = cont().toStack(
                cont -> Primitives._x_defineList(
                        e,
                        expected,
                        Scut.as( Cons.class, parse( "(1 2 3 4 5)" ) ),
                        cont ) );

        assertEqualq( expected, result );
        assertEqualq( e.get( s("a") ), i(1) );
        assertEqualq( e.get( s("b") ), i(2) );
    }

    @Test
    public void x_map() throws Exception
    {
        try
        {
            var iev = scriptEngine().getInteraction();

            Cons expected = parse( "(b d f)" ).as( Cons.class );

            var result = cont().toStack(
                    cont -> Primitives._x_map(
                            iev,
                            s("cadr"),
                            Scut.as( Cons.class, parse( "((a b)(c d)(e f))" ) ),
                            cont ) );

            assertEqualq( expected, result );
        }
        catch ( Exception e )
        {
            fail( e.toString() );
        }
    }
}
