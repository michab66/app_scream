/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */

package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

import de.michab.scream.Cons;
import de.michab.scream.Continuation;
import de.michab.scream.FirstClassObject;
import de.michab.scream.Lambda;
import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamEvaluator;
import de.michab.scream.ScreamException.Code;
import de.michab.scream.util.Scut;
import urschleim.Holder;

public class QuoteTest extends ScreamBaseTest
{
    @Test
    public void quoteTest_0() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var e = se.getInteraction();

        var fco =
                readSingleExpression( "(quote a)", Cons.class );
        Lambda l = Scut.as(
                Lambda.class,
                FirstClassObject._compile( fco, e ) );

        Holder<FirstClassObject> r = new Holder<>( Cons.NIL );

        l.evaluate( e, Continuation.endCall( r::set ) ).run();

        assertEquals( s("a"), r.get() );
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
