/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.pops;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

import de.michab.scream.Continuation;
import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamException.Code;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Operation;

public class SyntaxCaseTest extends ScreamBaseTest
{
    @Test
    public void exists() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                case
                """ );
        assertInstanceOf( Operation.class, result );
    }


    @Test
    public void syntaxCaseTest() throws Exception
    {
        Cons opCall = readSingleExpression(
                """
                (case (* 2 3)
                 ((2 3 5 7) 'prime)
                 ((1 4 6 8 s) 'composite))
                """,
                Cons.class );

        FirstClassObject r = Continuation.toStack(
                c -> opCall.evaluate( scriptEngine().getInteraction(), c ) );

        assertEquals(
                s("composite"),
                r );
    }

    @Test
    public void syntaxCaseTest_2() throws Exception
    {
        Cons opCall = readSingleExpression(
                """
                (case (+ 3 4)
                 ((2 3 5 7) 'prime)
                 ((1 4 6 8) 'composite))
                """,
                Cons.class );

        FirstClassObject r = Continuation.toStack(
                c -> opCall.evaluate( scriptEngine().getInteraction(), c ) );

        assertEquals(
                s("prime"),
                r );
    }

    /**
     * Cons error: Duplicate symbol.
     */
    @Test
    public void syntaxCaseError_duplicateSingleClause() throws Exception
    {
        Cons opCall = readSingleExpression(
                """
                (case (+ 3 4)
                 ((2 3 5 7 7) 'prime)
                 ((1 4 6 8) 'composite))
                """,
                Cons.class );

        try
        {
            @SuppressWarnings("unused")
            FirstClassObject fco = Continuation.toStack(
                    c -> opCall.evaluate( scriptEngine().getInteraction(), c ) );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.DUPLICATE_ELEMENT, rx.getCode() );
            assertEquals( i(7), rx.getArguments()[0] );
        }
    }

    /**
     * Cons error: Duplicate symbol.
     */
    @Test
    public void syntaxCaseError_duplicateAcrossClause() throws Exception
    {
        Cons opCall = readSingleExpression(
                """
                (case (+ 3 4)
                 ((2 3 5 7) 'prime)
                 ((1 4 6 8 7) 'composite))
                """,
                Cons.class );

        try
        {
            @SuppressWarnings("unused")
            FirstClassObject fco = Continuation.toStack(
                    c -> opCall.evaluate( scriptEngine().getInteraction(), c ) );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.DUPLICATE_ELEMENT, rx.getCode() );
            assertEquals( i(7), rx.getArguments()[0] );
        }
    }

    /**
     * Cons error: Duplicate symbol.
     */
    @Test
    public void syntaxCaseError_noListCarClause() throws Exception
    {
        var badClause = parse("(\"fail\" 'prime)");

        Cons opCall = readSingleExpression(
                """
                (case (+ 3 4)
                 ("fail" 'prime)
                 ((1 4 6 8 7) 'composite))
                """,
                Cons.class );

        try
        {
            @SuppressWarnings("unused")
            FirstClassObject fco = Continuation.toStack(
                    c -> opCall.evaluate( scriptEngine().getInteraction(), c ) );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.BAD_CLAUSE, rx.getCode() );
            assertEqualq( badClause, (FirstClassObject)rx.getArguments()[0] );
        }
    }
}
