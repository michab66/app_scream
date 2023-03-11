/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamException.Code;

/**
 * r7rs 4.2.2 Binding constructs, p16
 */
public class LetTest extends ScreamBaseTest
{
    @Test
    public void letTest_1() throws Exception
    {
        try
        {
            // x is not defined in second init in
            // difference to let*.
            scriptEngine().evalFco(
                """
                (let
                ((x 2) (y x))
                y)
                """ );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.SYMBOL_NOT_DEFINED, rx.getCode() );
            assertEquals( s("x"), rx.getArgument( 0 ) );
        }
    }

    @Test
    public void letTest_2() throws Exception
    {
        try
        {
            // Evaluation of y must fail.
            scriptEngine().evalFco(
                """
                (let
                ((x 2) (z 3))
                y)
                """ );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.SYMBOL_NOT_DEFINED, rx.getCode() );
            assertEquals( s("y"), rx.getArgument( 0 ) );
        }
    }

    @Test
    @Disabled
    public void letTest_3() throws Exception
    {
        try
        {
            // r7rsb: It is an error for a <variable> to
            // appear more than once in the list of variables being bound.
            scriptEngine().evalFco(
                """
                (let
                ((x 2) (x 3))
                8)
                """ );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.DUPLICATE_ELEMENT, rx.getCode() );
            assertEquals( s("x"), rx.getArgument( 0 ) );
        }
    }

}
