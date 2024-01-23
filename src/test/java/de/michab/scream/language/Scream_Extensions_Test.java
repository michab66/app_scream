/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.Assert.assertArrayEquals;

import org.junit.jupiter.api.Test;
import org.smack.util.io.Redirect;
import org.smack.util.io.Redirect.StdStream;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Cons;

/**
 * scream extensions
 */
public class Scream_Extensions_Test extends ScreamBaseTest
{
    @Test
    public void cons_s_scream$circularq()
            throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(define circular0 (scream:make-circular! (list 1)))",
                Cons.NIL );
        t.expectFco(
                "(scream:circular? circular0)",
                bTrue );
        t.expectFco(
                "(define circular1 (scream:make-circular! (list 2)))",
                Cons.NIL );
        t.expectFco(
                "(scream:circular? circular0 circular1)",
                bTrue );
        t.expectFco(
                "(scream:circular? circular0 circular1 (list 3))",
                bFalse );
    }

    @Test
    public void basic_s_scream$display_ln()
            throws Exception
    {
        try ( var stdout = new Redirect( StdStream.out ) )
        {
            expectFco(
                    "(scream:display-ln 1)",
                    Cons.NIL );

            assertArrayEquals(
                    "1 \n".getBytes(),
                    stdout.contentRaw() );
        }
    }

    @Test
    public void ScreamEvaluator_scream$eval()
            throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(scream:eval '(+ 300 10 3))",
                i313 );
        t.expectError(
                "(scream:eval 1 2)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
        t.expectError(
                "(scream:eval)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
        t.expectError(
                "(scream:eval '(1 2))",
                Code.CALLED_NON_PROCEDURAL );
        t.expectError(
                "(scream:eval '(+ 1 2 3 4 5 6 7 8 9 'donald))",
                Code.TYPE_ERROR );
    }

    @Test
    public void basic_s_scream$transform()
            throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(define (plus1 x) (+ 1 x))",
                Cons.NIL );
        t.expectFco(
                "(scream:transform plus1 '( 1 2 3 4 5 6 7 8 9 10 11 12 313))",
                "(2 3 4 5 6 7 8 9 10 11 12 13 314)" );
        t.expectFco(
                "(scream:transform plus1 '())",
                Cons.NIL );
    }

    @Test
    public void basic_s_scream$make_transitive()
            throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(define numbers? (scream:make-transitive number?))",
                Cons.NIL );
        t.expectFco(
                "(numbers? 1 2 3 4 5)",
                bTrue );
        t.expectFco(
                "(numbers?)",
                bFalse );
        t.expectFco(
                "(numbers? 1 'a 3 4 5)",
                bFalse );
    }
}
