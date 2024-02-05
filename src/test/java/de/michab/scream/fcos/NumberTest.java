/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.RuntimeX.Code;

public class NumberTest
{
    @Test
    public void makeExact() throws Exception
    {
        assertEquals(
                0,
                Number.make( 0 ).asLong() );
        assertTrue(
                Number.make( 0 ) == Number.make( 0 ) );
        assertTrue(
                Number.make( 0 ).isExact() );
        assertInstanceOf(
                Int.class,
                Number.make( 0 ) );

        // Double values are converted to long if they represent an
        // integer and exactness is requested.
        assertEquals(
                0,
                Number.make( .0 ).asLong() );
        assertTrue(
                Number.make( .0, true ) == Number.make( 0 ) );
        assertTrue(
                Number.make( .0, true ).isExact() );
        assertInstanceOf(
                Int.class,
                Number.make( .0, true ) );

        // Error expected if exactness is requested for a double that
        // is not an integer.
        RuntimeX rx =
        assertThrows(
                RuntimeX.class,
                () -> {
                    Number.make( Math.PI, true );
                } );
        assertEquals( Code.TYPE_ERROR, rx.getCode() );
    }

    @Test
    public void makeInexact() throws Exception
    {
// TODO caching for common reals is sensible?
//        assertTrue(
//                Number.make( 0 ) == Number.make( 0 ) );
        // Default for double is inexact.
        assertFalse(
                Number.make( .0 ).isExact() );
        assertInstanceOf(
                Real.class,
                Number.make( .0 ) );

        assertFalse(
                Number.make( 0, false ).isExact() );
        assertInstanceOf(
                Real.class,
                Number.make( 0, false ) );
    }
}
