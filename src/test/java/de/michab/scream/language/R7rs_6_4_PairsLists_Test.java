/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream.language;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Bool;
import de.michab.scream.fcos.Cons;

/**
 * rsr7 6.4 Pairs and lists.
 *
 * @author MICBINZ
 */
public class R7rs_6_4_PairsLists_Test extends ScreamBaseTest
{
    /**
     * p41
     */
    @Test
    public void schemeListNotation() throws Exception
    {
        var t = makeTester();

        t.expectFco(
            "'(a . (b . (c . (d . (e . ())))))",
            "(a b c d e)" );
        t.expectFco(
            "'(a . (b . (c . d)))",
            "(a b c . d)" );
    }

    /**
     * p41
     */
    @Test
    public void pairQ() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(pair? '(a . b))",
                bTrue );
        t.expectFco(
                "(pair? '(a b c))",
                bTrue );
        t.expectFco(
                "(pair? '())",
                bFalse );
        t.expectFco(
                "(pair? '#(a b))",
                bFalse );
    }

    /**
     * p41
     */
    @Test
    public void cons() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(cons 'a '())",
                "(a)" );
        t.expectFco(
                "(cons '(a) '(b c d))",
                "((a) b c d)" );
        t.expectFco(
                """
                (cons "a" '(b c))
                ""","""
                ("a" b c)
                """ );
        t.expectFco(
                "(cons 'a 3)",
                "(a . 3)" );
        t.expectFco(
                "(cons '(a b) 'c)",
                "((a b) . c)" );
    }

    /**
     * p42
     */
    @Test
    public void car() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(car '(1))",
                i1 );
        t.expectFco(
                "(car '(1 . 2))",
                i1 );
        t.expectFco(
                "(car '(1 2))",
                i1 );
        var rx = t.expectError(
                "(car '())",
                Code.TYPE_ERROR );
        assertEqualq(
                s("car"),
                rx.getOperationName() );
    }

    /**
     * p42
     */
    @Test
    public void cdr() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(cdr '(1))",
                Cons.NIL );
        t.expectFco(
                "(cdr '(1 . 2))",
                i2 );
        t.expectFco(
                "(cdr '(1 2))",
                "(2)" );
        var rx = t.expectError(
                "(cdr '())",
                Code.TYPE_ERROR );
        assertEqualq(
                s("cdr"),
                rx.getOperationName() );
    }

    /**
     * p42
     */
    @Test
    public void nullQ() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(null? '(a b c))",
                bFalse );
        t.expectFco(
                "(null? '())",
                Bool.T );
        t.expectFco(
                "(list? '(a b c))",
                Bool.T );
    }

    /**
     * p42
     */
    @Test
    public void listQ() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                """
                (list? '())
                """,
                Bool.T );
        t.expectFco(
                """
                (list? '(a . c))
                """,
                Bool.F );
        t.expectFco(
                """
                (let ((x (list 'a)))
                  (set-cdr! x x)
                  (list? x))
                """,
                Bool.F );
    }

    /**
     * p42
     */
    @Test
    public void make_list() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(make-list 2 3)",
                "(3 3)" );
        t.expectFco(
                "(make-list 2)",
                "(() ())" );
        t.expectError(
                "(make-list 'symbol)",
                Code.TYPE_ERROR );
    }

    /**
     * p42
     */
    @Test
    public void schemeList() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(list 'a (+ 3 4) 'c)",
                "(a 7 c)" );
        t.expectFco(
                "(list)",
                "()" );
    }

    /**
     * p42
     */
    @Test
    public void length() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(length '(a b c))",
                i(3) );
        t.expectFco(
                "(length '(a (b) (c d e)))",
                i(3) );
        t.expectFco(
                "(length '())",
                i(0) );
        var rx = t.expectError(
                "(length '(1 . 2))",
                Code.EXPECTED_PROPER_LIST );
        assertEqualq(
                s("length"),
                rx.getOperationName() );
    }

    /**
     * p42
     */
    @Test
    public void schemeAppend_1() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(append '(x) '(y))",
                "(x y)" );
        t.expectFco(
                "(append '(a) '(b c d))",
                "(a b c d)" );
        t.expectFco(
                "(append '(a (b)) '((c)))",
                "(a (b) (c))" );
        t.expectFco(
                "(append '(a b) '(c . d))",
                "(a b c . d)" );
        t.expectFco(
                "(append '() 'a)",
                s( "a" ));
        t.expectFco(
                "(append)",
                Cons.NIL );
        t.expectFco(
                "(append 'a)",
                s( "a" ) );
        t.expectFco(
                "(append 1)",
                i( 1 ) );
        t.expectFco(
                "(append '())",
                Cons.NIL );
        t.expectFco(
                "(append '(a) 1)",
                "(a . 1)" );
        t.expectFco(
                "(append '(a) 'b)",
                "(a . b)" );
        t.expectFco(
                "(append '() '(1) 2)",
                "(1 . 2)" );
        t.expectFco(
                "(append '() '(1) '(2) 3)",
                "(1 2 . 3)" );
        t.expectError(
                "(append '() 1 2)",
                Code.TYPE_ERROR );
    }

    /**
     * p42
     */
    @Test
    public void reverse() throws Exception
    {
        var t = makeTester();

        t.expectFco(
            "(reverse '(a b c))",
            "(c b a)" );
        t.expectFco(
            "(reverse '(a (b c) d (e (f))))",
            "((e (f)) d (b c) a)" );
        t.expectFco(
            "(reverse '())",
            Cons.NIL );
        t.expectFco(
                "(reverse '(313))",
                "(313)" );

        var rx = t.expectError(
                "(reverse '(313 . 314))",
                Code.EXPECTED_PROPER_LIST );
        assertEqualq(
                s("reverse"),
                rx.getOperationName() );
    }

    /**
     *
     */
    @Test
    public void list_tail() throws Exception
    {
        var t = makeTester();

        t.expectFco(
           "(list-tail '(a b c d) 2)",
           "(c d)" );
        t.expectFco(
              "(list-tail '(a b c d) 0)",
              "(a b c d)" );
        t.expectFco(
                "(list-tail '(a b c d) 4)",
                Cons.NIL );

        assertEqualq(
                s("list-tail"),
                t.expectError(
                        "(list-tail 0 4)",
                        Code.TYPE_ERROR ).getOperationName() );
    }

    /**
     * p43
     */
    @Test
    public void list_ref() throws Exception
    {
        var t = makeTester();

        t.expectFco( "(list-ref '(a b c d) 2)",
                "c" );
        t.expectFco(
                "(list-ref '(a b c d) (exact (round 1.8)))",
                s("c") );

        assertEqualq(
                s("list-ref"),
                t.expectError(
                        "(list-ref 0 4)",
                        Code.TYPE_ERROR ).getOperationName() );
    }

    /**
     * p43
     */
    @Test
    public void list_setE() throws Exception
    {
        var t = makeTester();

        expectFco( """
                (let ((ls (list 'one 'two 'five!)))
                  (list-set! ls 2 'three)
                    ls)
                """,
                "(one two three)" );

        t.expectError(
                "(list-set! '(0 1 2) 1 'oops)",
                Code.CANNOT_MODIFY_CONSTANT );
        t.expectError(
                "(list-set! '(0 1 2) 7 'oops)",
                Code.INDEX_OUT_OF_BOUNDS );
        t.expectError(
                "(list-set! '(0 1 2) -1 'oops)",
                Code.INDEX_OUT_OF_BOUNDS );
        t.expectError(
                "(list-set! (scream:make-circular! (list 0 1 2)) 1 'oops)",
                Code.EXPECTED_PROPER_LIST );
    }

    /**
     * p43
     */
    @Test
    public void memq_1() throws Exception
    {
        expectFco(
                "(memq 'a '(a b c))",
                "(a b c)" );
    }

    /**
     * p43
     */
    @Test
    public void memq_empty() throws Exception
    {
        expectFco(
                "(memq 'a '())",
                bFalse );
    }

    /**
     * p43
     */
    @Test
    public void memq_2() throws Exception
    {
        expectFco( "(memq 'b '(a b c))",
                "(b c)" );
    }

    /**
     * p43
     */
    @Test
    public void memq_3() throws Exception
    {
        expectFco(
                "(memq 'a '(b c d))",
                bFalse );
    }

    /**
     * p43
     */
    @Test
    public void memq_4() throws Exception
    {
        expectFco(
                "(memq (list 'a) '(b (a) c))",
                bFalse );
    }

    /**
     * p43
     */
    @Test
    public void memq_4_error() throws Exception
    {
        expectError( """
                (memq 'a 0)
                """,
                Code.TYPE_ERROR );
    }

    /**
     * p43
     */
    @Test
    public void memv_empty() throws Exception
    {
        expectFco(
                "(memv 101 '())",
                bFalse );
    }

    /**
     * p43
     */
    @Test
    public void memv_1() throws Exception
    {
        expectFco( """
                (memv 101 '(100 101 102))
                """,
                "(101 102)" );
    }

    /**
     * p43
     */
    @Test
    public void memv_error() throws Exception
    {
        expectError( """
                (memv 101 "(100 101 102)")
                """,
                Code.TYPE_ERROR );
    }

    /**
     * p43
     */
    @Test
    public void member_empty() throws Exception
    {
        expectFco(
                "(member 0 '())",
                bFalse );
    }

    /**
     * p43
     */
    @Test
    public void member_empty_comp() throws Exception
    {
        expectFco(
                "(member 0 '() =)",
                bFalse );
    }

    /**
     * p43
     */
    @Test
    public void member_1() throws Exception
    {
        expectFco( """
                (member (list 'a)
                '(b (a) c))
                """,
                "((a) c)" );
    }

    /**
     * p43
     */
    @Test
    public void member_2() throws Exception
    {
        expectFco( """
                (member "B"
                 '("a" "b" "c")
                 string-ci=?)
                """,
                """
                 ("b" "c")
                """ );
    }

    /**
     * p43
     *
     * TODO this is an example where the error message should
     * refer to the offending operation (in this case
     * (string-ci=? ...)).
     */
    @Test
    public void member_1_error() throws Exception
    {
        expectError( """
                (member "B"
                 '("a", "b", "c")
                 string-ci=?)
                """,
                Code.TYPE_ERROR );
    }

    /**
     * p43
     */
    @Test
    public void member_2_error() throws Exception
    {
        expectError( """
                (member "B"
                 'donald
                 string-ci=?)
                """,
                Code.TYPE_ERROR);
    }

    /**
     * p43
     */
    @Test
    public void member_3_error() throws Exception
    {
        expectError( """
                (member "B"
                 'donald
                 7)
                """,
                Code.TYPE_ERROR );
    }

    /**
     * p43
     */
    @Test
    public void memq_5() throws Exception
    {
        expectFco( """
                (memq 101 '(100 101 102))
                """,
                bFalse );
    }

    /**
     * p43
     */
    @Test
    public void assq_1() throws Exception
    {
        var t = makeTester();

        t.execute( "(define e '((a 1)(b 2)(c 3)))" );

        t.expectFco(
                "(assq 'a e)",
                "(a 1)" );
        t.expectFco(
                "(assq 'b e)",
                "(b 2)" );
        t.expectFco(
                "(assq 'd e)",
                bFalse );
    }

    /**
     * p43
     */
    @Test
    public void assv() throws Exception
    {
        expectFco(
                "(assv 5 '((2 3)(5 7) (11 13)))",
                "(5 7)" );
    }

    /**
     * p43
     */
    @Test
    public void assoc() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(assoc (list 'a) '(((a)) ((b)) ((c))))",
                "((a))" );
        t.expectFco(
                "(assoc 2.0 '((1 1) (2 4) (3 9)) =)",
                "(2 4)" );
    }

    /**
     * p43
     */
    @Test
    public void list_copy() throws Exception
    {
        var t = makeTester();

        t.expectError(
"""
        (define a '(1 8 2 8))
        (set-car! a '3)
""",
        Code.CANNOT_MODIFY_CONSTANT );

        t.expectFco(
"""
        (define a '(1 8 2 8))
        (define b (list-copy a))
        (set-car! b '3)
        b
""",
        "(3 8 2 8)" );

        // Improper list.
        t.expectFco(
                "(list-copy '(1 2 . 4))",
                "(1 2 . 4)" );

        // "An obj which is not a list is
        // returned unchanged."
        t.expectFco(
                "(list-copy 5)",
                i(5));

        // "It is an error if obj is a circular list."
        var rx = t.expectError(
                "(list-copy (scream:make-circular! (list 1 2)))",
                Code.ILLEGAL_ARGUMENT );
        assertEqualq(
                s("list-copy"),
                rx.getOperationName() );
    }
}
