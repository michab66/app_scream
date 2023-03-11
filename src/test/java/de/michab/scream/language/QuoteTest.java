/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.Scream;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamEvaluator;
import de.michab.scream.ScreamException.Code;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.FirstClassObject;

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
        ScreamEvaluator se = scriptEngine();

        var e = se.getInteraction();

        var fco =
                readSingleExpression( "(quote a)", Cons.class );

        FirstClassObject result = Scream.toStack(
                c -> fco.evaluate( e, c ) );

        assertEquals( s("a"), result );
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
        ScreamEvaluator se = scriptEngine();

        try
        {
            se.evalFco(
                """
                (quote)
                """ );
        }
        catch ( RuntimeX rx )
        {
            assertNotNull( rx.getMessage() );
            assertEquals( Code.WRONG_NUMBER_OF_ARGUMENTS, rx.getCode() );
        }
    }

    @Test
    public void quoteTest_3() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        try
        {
            se.evalFco(
                """
                (quote 8 'q)
                """ );
        }
        catch ( RuntimeX rx )
        {
            assertNotNull( rx.getMessage() );
            assertEquals( Code.WRONG_NUMBER_OF_ARGUMENTS, rx.getCode() );
        }
    }

    @Test
    public void quoteListTest() throws Exception
    {
        expectFco(
                """
                '(1 2 . 3)
                """,
                parse( "(1 2 . 3)" ) );
    }
}
