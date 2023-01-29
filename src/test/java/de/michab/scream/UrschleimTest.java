/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 2022-2023 Michael G. Binz
 */
package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Environment;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.SchemeInteger;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.frontend.SchemeParser;

public class UrschleimTest extends ScreamBaseTest
{
    @Test
    public void typeIntegerTest() throws Exception
    {
        FirstClassObject r = Scream.toStack(
                c -> i313.evaluate( null, c ) );

        assertEquals( "313", r.toString() );
    }

    @Test
    public void typeSymbolTest() throws Exception
    {
        var symbol = s( "car" );
        var env = new Environment();
        env.define( symbol, i313 );

        FirstClassObject r = Scream.toStack(
                c-> symbol.evaluate( env, c ) );

        assertEquals( "313", r.toString() );
    }

    @Test
    public void typeSymbolErrorTest() throws Exception
    {
        var symbol = Symbol.createObject( "car" );

        try
        {
            Scream.toStack(
                    c -> symbol.evaluate( new Environment(), c ) );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals(
                    ScreamException.Code.SYMBOL_NOT_DEFINED,
                    rx.getCode() );
        }
    }

    @Test
    public void operationTest() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (%syntax (xquote value) value)
                (xquote micbinz)
                """ );
        assertEquals( s( "micbinz" ), result );

        FirstClassObject opCall =
                new SchemeParser( "(xquote not-defined-yet)" ).getExpression();
        assertInstanceOf( Cons.class, opCall );

        FirstClassObject r = Scream.toStack(
                c -> opCall.evaluate( se.getInteraction(), c ) );

        assertNotNull(
                r );
        assertInstanceOf(
                Symbol.class,
                r );
        assertEquals(
                Symbol.createObject( "not-defined-yet" ),
                r );
    }

    @Test
    public void procedureTest() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (define (add2 value) (+ value 2))
                (add2 311)
                """ );
        assertEquals( i313, result );

        FirstClassObject opCall =
                new SchemeParser( "(add2 311)" ).getExpression();
        assertInstanceOf( Cons.class, opCall );

        FirstClassObject r = Scream.toStack(
                c -> opCall.evaluate( se.getInteraction(), c ) );

        assertNotNull(
                r );
        assertInstanceOf(
                SchemeInteger.class,
                r );
        assertEquals(
                ScreamBaseTest.i313,
                r );
    }

    @Test
    public void syntaxQuoteTest() throws Exception
    {
        _contTest(
                "'lumumba'",
                s("lumumba") );
    }

    @Test
    public void syntaxAssignmentTest() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var env = se.getInteraction();
        env.define( s313, i1  );

        FirstClassObject opCall =
                new SchemeParser( "(set! threethirteen 313)" ).getExpression();
        assertInstanceOf( Cons.class, opCall );

        FirstClassObject r = Scream.toStack(

                c -> opCall.evaluate( se.getInteraction(), c ) );

        assertEquals(
                Cons.NIL,
                r );
        assertEquals(
                i313,
                env.get( s313 ) );
    }

}
