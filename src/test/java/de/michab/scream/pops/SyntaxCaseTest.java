/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.pops;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamException.Code;
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
        expectFco(
                """
                (case (* 2 3)
                 ((2 3 5 7) 'prime)
                 ((1 4 6 8 s) 'composite))
                """,
                s("composite") );
    }

    @Test
    public void syntaxCaseTest_2() throws Exception
    {
        expectFco(
                """
                (case (+ 3 4)
                 ((2 3 5 7) 'prime)
                 ((1 4 6 8) 'composite))
                """,
                s("prime") );
    }

    /**
     * Cons error: Duplicate symbol.
     */
    @Test
    public void syntaxCaseError_duplicateSingleClause() throws Exception
    {
        var rx = expectError(
                """
                (case (+ 3 4)
                 ((2 3 5 7 7) 'prime)
                 ((1 4 6 8) 'composite))
                """,
                Code.DUPLICATE_ELEMENT );
        assertEquals( i(7), rx.getArguments()[0] );
    }

    /**
     * Cons error: Duplicate symbol.
     */
    @Test
    public void syntaxCaseError_duplicateAcrossClause() throws Exception
    {
        var rx = expectError(
                """
                (case (+ 3 4)
                 ((2 3 5 7) 'prime)
                 ((1 4 6 8 7) 'composite))
                """,
                Code.DUPLICATE_ELEMENT );
        assertEquals( i(7), rx.getArguments()[0] );
    }

    /**
     * Cons error: Duplicate symbol.
     */
    @Test
    public void syntaxCaseError_noListCarClause() throws Exception
    {
        var rx = expectError(
                """
                (case (+ 3 4)
                 ("fail" 'prime)
                 ((1 4 6 8 7) 'composite))
                """,
                Code.BAD_CLAUSE );
        var badClause = parse("(\"fail\" 'prime)");
        assertEqualq( badClause, (FirstClassObject)rx.getArguments()[0] );
    }
}
