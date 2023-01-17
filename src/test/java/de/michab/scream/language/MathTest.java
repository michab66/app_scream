/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import org.junit.jupiter.api.Test;

import de.michab.scream.Continuation;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamEvaluator;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.SchemeBoolean;

public class MathTest extends ScreamBaseTest
{
    @Test
    public void mathEquals_t() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (= 313 313)
                """ );
        assertEquals( SchemeBoolean.T, result );
    }
    @Test
    public void mathEquals_f() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (= 121 313)
                """ );
        assertEquals( SchemeBoolean.F, result );
    }

    @Test
    public void mathEquals_t2() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        se.evalFco( "(define i 121)" );
        assertEqualq( i(121), se.evalFco( "i" ) );

        var result = se.evalFco(
                """
                (= i 121)
                """ );
        assertEquals( SchemeBoolean.T, result );
    }

    @Test
    public void _equalsCompile() throws Exception
    {
        String code = "(= 121 121)";

        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                code );
        assertEquals( SchemeBoolean.T, result );

        FirstClassObject opCall = parse(
                code );
        assertInstanceOf( Cons.class, opCall );

        FirstClassObject r = Continuation.toStack(
                se.getInteraction(),
                opCall::evaluate );

        assertEqualq(
                SchemeBoolean.T,
                r );
    }

    @Test
    public void _equalsCompile2() throws Exception
    {
        String code = "(= i 121)";

        ScreamEvaluator se = scriptEngine();
        se.evalFco( "(define i 121)" );
        var checkIfSet = se.evalFco( "i" );
        assertEqualq( i(121), checkIfSet );

        var result = se.evalFco(
                code );
        assertEquals( SchemeBoolean.T, result );

        FirstClassObject opCall = parse(
                code );
        assertInstanceOf( Cons.class, opCall );

        FirstClassObject r = Continuation.toStack(
                se.getInteraction(),
                opCall::evaluate );

        assertEqualq(
                SchemeBoolean.T,
                r );
    }
}
