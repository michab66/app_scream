/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */

package de.michab.scream.pops;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import javax.script.ScriptException;

import org.junit.jupiter.api.Test;

import de.michab.scream.Cons;
import de.michab.scream.FirstClassObject;
import de.michab.scream.Lambda;
import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamEvaluator;
import de.michab.scream.ScreamException.Code;

public class QuoteTest extends ScreamBaseTest
{
    @Test
    public void quoteTest_0() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var e = se.getInteraction();

        var x =readSingleExpression( "(quote a)", Cons.class );

        Lambda l = FirstClassObject.as( Lambda.class, x.compile( e ) );

        var result =
        FirstClassObject.evaluate( l, null );
        assertEquals( s("a"), result );
    }

    @Test
    public void quoteTest_1() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (quote a)
                """ );
        assertEquals( s("a"), result );
    }

    @Test
    public void quoteTest_1_5() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                ' a
                """ );
        assertEquals( s("a"), result );
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
        catch ( ScriptException e )
        {
            RuntimeX rx = (RuntimeX)e.getCause();
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
        catch ( ScriptException e )
        {
            RuntimeX rx = (RuntimeX)e.getCause();
            assertNotNull( rx.getMessage() );
            assertEquals( Code.WRONG_NUMBER_OF_ARGUMENTS, rx.getCode() );
        }
    }

    @Test
    public void _quoteTest() throws Exception
    {
        _contTest(
                """
                (quote micbinz)
                """,
                s( "micbinz" ) );
        _contTest(
                """
                'micbinz
                """,
                s( "micbinz" ) );
    }
}
