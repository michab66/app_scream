/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;

public class QuoteTest extends ScreamBaseTest
{
    @Test
    public void constantness() throws Exception
    {
        assertTrue( scriptEngine().evalFco( "(quote a)" ).isConstant() );
        assertTrue( scriptEngine().evalFco( "'(quote a)" ).isConstant() );
        assertTrue( scriptEngine().evalFco( "'#(vector a)" ).isConstant() );
    }

    @Test
    public void quoteTest_0() throws Exception
    {
        expectFco( "(quote a)", s("a") );
    }

    @Test
    public void quoteTest_1() throws Exception
    {
        expectFco( "(quote a)", s("a") );
    }

    @Test
    public void quoteTest_1_5() throws Exception
    {
        expectFco( "'a", s("a") );
    }

    @Test
    public void quoteTest_2() throws Exception
    {
        expectError(
                """
                (quote)
                """,
                Code.WRONG_NUMBER_OF_ARGUMENTS );
    }

    @Test
    public void quoteTest_3() throws Exception
    {
        expectError(
                """
                (quote 8 'q)
                """,
                Code.WRONG_NUMBER_OF_ARGUMENTS );
    }

    @Test
    public void quoteListTest() throws Exception
    {
        expectFco(
                """
                '(1 2 . 3)
                """,
                "(1 2 . 3)" );
    }
}
