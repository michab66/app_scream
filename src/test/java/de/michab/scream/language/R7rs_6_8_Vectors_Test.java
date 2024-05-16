/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.language;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;

public class R7rs_6_8_Vectors_Test extends ScreamBaseTest
{
    @Test
    public void vectorQ() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(vector? #(0 (2 2 2 2) \"Anna\"))",
                bTrue );
        t.expectFco(
                "(vector? '())",
                bFalse );
        t.expectFco(
                "(vector? 1)",
                bFalse );
        t.expectFco(
                "(vector? '(1 2 3))",
                bFalse );
        t.expectFco(
                "(vector? 'symbol)",
                bFalse );
        t.expectFco(
                "(vector? \"string\")",
                bFalse );
        t.expectError(
                "(vector?)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
    }

    @Test
    public void make_vector() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(make-vector 0)",
                "#()" );
        t.expectFco(
                "(make-vector 3)",
                "#(()()())" );
        t.expectFco(
                "(make-vector 3 'x)",
                "#(x x x)" );
        t.expectError(
                "(make-vector \"3\")",
                Code.TYPE_ERROR );
        // TODO
//        t.expectError(
//                "(make-vector -3)",
//                Code.TYPE_ERROR );
    }

    @Test
    public void vector() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(vector)",
                "#()" );
        t.expectFco(
                "(vector 'a 'b 'c)",
                "#(a b c)" );
    }

    @Test
    public void vector_length() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(vector-length #())",
                i(0) );
        t.expectFco(
                "(vector-length #(1 2 3))",
                i3 );
    }

    @Test
    public void vector_ref() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(vector-ref #(1 1 2 3 5 8 13 21) 5)",
                i(8) );
        t.expectFco(
                """
                (vector-ref #(1 1 2 3 5 8 13 21)
                            (exact
                             (round (* 2 (acos -1)))))
                """,
                i(13) );
        t.expectFco(
                "(vector-ref #(1 2 3) 0)",
                i1 );
        t.expectFco(
                "(vector-ref #(1 2 3) 1)",
                i2 );
        t.expectFco(
                "(vector-ref #(1 2 3) 2)",
                i3 );

        t.expectError(
                "(vector-ref #(1 2 3) 3)",
                Code.INDEX_OUT_OF_BOUNDS );
        assertEqualq(
                s("vector-ref"),
                t.expectError(
                        "(vector-ref 0 3)",
                        Code.TYPE_ERROR ).getOperationName() );
        assertEqualq(
                s("vector-ref"),
                t.expectError(
                        "(vector-ref #(1 2 3) 'x)",
                        Code.TYPE_ERROR ).getOperationName() );
    }

    @Test
    public void vector_setQ() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                """
                (let ((vec (vector 0 '(2 2 2 2) "Anna")))
                  (vector-set! vec 1 '("Sue" "Sue"))
                  vec)
                """,
                """
                #(0 ("Sue" "Sue") "Anna")
                """ );
        t.expectError(
                """
                (vector-set! '#(0 1 2) 1 "doe")
                """,
                Code.CANNOT_MODIFY_CONSTANT );
    }

    @Test
    public void vector_2_list() throws Exception
    {
        var t = makeTester();

        t.execute(  "(define v #(1 2 3 4))" );

        t.expectFco(
                "(vector->list v)",
                "(1 2 3 4)" );
        // TODO
//        t.expectFco(
//                "(vector->list v 1)",
//                "(2 3 4)" );
    }
}
