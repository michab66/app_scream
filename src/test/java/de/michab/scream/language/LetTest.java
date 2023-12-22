/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;

/**
 * r7rs 4.2.2 Binding constructs, p16
 */
public class LetTest extends ScreamBaseTest
{
    @Test
    public void letTest_1() throws Exception
    {
        var rx = expectError(
                """
            ; x is not defined in second init in
            ; difference to let*.
                (let
                ((x 2) (y x))
                y)
                """,
                Code.SYMBOL_NOT_DEFINED );
        assertEquals( s("x"), rx.getArgument( 0 ) );
    }

    @Test
    public void letTest_2() throws Exception
    {
        var rx = expectError(
                """
            ; Evaluation of y must fail.
                (let
                ((x 2) (z 3))
                y)
                """,
                Code.SYMBOL_NOT_DEFINED);
            assertEquals( s("y"), rx.getArgument( 0 ) );
    }

    @Test
    public void letTest_3() throws Exception
    {
        var rx = expectError(
                """
            ; r7rs: It is an error for a <variable> to
            ; appear more than once in the list of variables being bound.
                (let
                ((x 2) (x 3))
                8)
                """,
                Code.DUPLICATE_ELEMENT );
            assertEquals( s("x"), rx.getArgument( 0 ) );
    }
}
