/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream.language;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.SchemeBoolean;

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
    public void schemeListNotation_1() throws Exception
    {
        expectFco(
            "'(a . (b . (c . (d . (e . ())))))",
            "(a b c d e)" );
    }

    /**
     * p41
     */
    @Test
    public void schemeListNotation_2() throws Exception
    {
        expectFco(
            "'(a . (b . (c . d)))",
            "(a b c . d)" );
    }

    /**
     * p41
     */
    @Test
    public void schemePair_1() throws Exception
    {
        expectFco(
            "(pair? '(a . b))",
            bTrue );
    }

    /**
     * p41
     */
    @Test
    public void schemePair_2() throws Exception
    {
        expectFco(
                "(pair? '(a b c))",
                bTrue );
    }

    /**
     * p41
     */
    @Test
    public void schemePair_3() throws Exception
    {
        expectFco(
                "(pair? '())",
                bFalse );
    }

    /**
     * p41
     */
    @Test
    public void schemePair_4() throws Exception
    {
        expectFco(
                "(pair? '#(a b))",
                bFalse );
    }

    /**
     * p41
     */
    @Test
    public void schemeCons_1() throws Exception
    {
        expectFco(
                "(cons 'a '())",
                "(a)" );
    }

    /**
     * p41
     */
    @Test
    public void schemeCons_2() throws Exception
    {
        expectFco(
                "(cons '(a) '(b c d))",
                "((a) b c d)" );
    }

    /**
     * p41
     */
    @Test
    public void schemeCons_3() throws Exception
    {
        expectFco(
                """
                (cons "a" '(b c))
                ""","""
                ("a" b c)
                """ );
    }

    /**
     * p41
     */
    @Test
    public void schemeCons_4() throws Exception
    {
        expectFco(
                "(cons 'a 3)",
                "(a . 3)" );
    }

    /**
     * p41
     */
    @Test
    public void schemeCons_5() throws Exception
    {
        expectFco(
                "(cons '(a b) 'c)",
                "((a b) . c)" );
    }

    /**
     * p42
     */
    @Test
    public void schemeNull_x1() throws Exception
    {
        expectFco(
                "(null? '(a b c))",
                bFalse );
    }

    /**
     * p42
     */
    @Test
    public void schemeNull_x2() throws Exception
    {
        expectFco(
                """
                (null? '())
                """,
                SchemeBoolean.T );
    }

    /**
     * p42
     */
    @Test
    public void schemeListQ_1() throws Exception
    {
        expectFco(
                """
                (list? '(a b c))
                """,
                SchemeBoolean.T );
    }

    /**
     * p42
     */
    @Test
    public void schemeListQ_2() throws Exception
    {
        expectFco(
                """
                (list? '())
                """,
                SchemeBoolean.T );
    }

    /**
     * p42
     */
    @Test
    public void schemeListQ_3() throws Exception
    {
        expectFco(
                """
                (list? '(a . c))
                """,
                SchemeBoolean.F );
    }

    /**
     * p42
     */
    @Test
    public void schemeListQ_4() throws Exception
    {
        expectFco(
                """
                (let ((x (list 'a)))
                  (set-cdr! x x)
                  (list? x))
                """,
                SchemeBoolean.F );
    }

    /**
     * p42
     */
    @Test
    public void schemeMake_List_1() throws Exception
    {
        expectFco(
                "(make-list 2 3)",
                "(3 3)" );
    }

    /**
     * p42
     */
    @Test
    public void schemeMake_List_x1() throws Exception
    {
        expectFco(
                "(make-list 2)",
                "(() ())" );
    }
    /**
     * p42
     */
    @Test
    public void schemeMake_List_error_1() throws Exception
    {
        expectError(
                "(make-list 'symbol)",
                Code.TYPE_ERROR );
    }

    /**
     * p42
     */
    @Test
    public void schemeList_1() throws Exception
    {
        expectFco(
                "(list 'a (+ 3 4) 'c)",
                "(a 7 c)" );
    }

    /**
     * p42
     */
    @Test
    public void schemeList_2() throws Exception
    {
        expectFco(
                "(list)",
                "()" );
    }

    /**
     * p42
     */
    @Test
    public void schemeLength_1() throws Exception
    {
        expectFco(
                """
                (length '(a b c))
                """,
                i(3) );
    }

    /**
     * p42
     */
    @Test
    public void schemeLength_2() throws Exception
    {
        expectFco(
                """
                (length '(a (b) (c d e)))
                """,
                i(3) );
    }

    /**
     * p42
     */
    @Test
    public void schemeLength_3() throws Exception
    {
        expectFco(
                """
                (length '())
                """,
                i(0) );
    }

    @Test
    public void schemeLength_improperFail() throws Exception
    {
        expectError(
                """
                (length '(a b . c))
                """,
                Code.EXPECTED_PROPER_LIST );
    }

    /**
     * p42
     */
    @Test
    public void schemeAppend_1() throws Exception
    {
        expectFco(
                "(append '(x) '(y))",
                "(x y)" );
    }

    /**
     * p42
     */
    @Test
    public void schemeAppend_2() throws Exception
    {
        expectFco(
                "(append '(a) '(b c d))",
                "(a b c d)" );
    }

    /**
     * p42
     */
    @Test
    public void schemeAppend_3() throws Exception
    {
        expectFco(
                "(append '(a (b)) '((c)))",
                "(a (b) (c))" );
    }

    /**
     * p42
     */
    @Test
    public void schemeAppend_4() throws Exception
    {
        expectFco(
                "(append '(a b) '(c . d))",
                "(a b c . d)" );
    }

    /**
     * p42
     */
    @Test
    public void schemeAppend_5() throws Exception
    {
        expectFco(
                "(append '() 'a)",
                s( "a" ));
    }

    /**
     * p42
     * If there are no arguments, the empty list is returned.
     */
    @Test
    public void schemeAppend_x1() throws Exception
    {
        expectFco(
                "(append)",
                Cons.NIL );
    }

    /**
     * p42
     * If there is exactly one argument, it is returned.
     */
    @Test
    public void schemeAppend_oneArgument() throws Exception
    {
        expectFco(
                "(append 'a)",
                s( "a" ) );
        expectFco(
                "(append 1)",
                i( 1 ) );
        expectFco(
                "(append '())",
                Cons.NIL );
    }

    /**
     * p42
     * Append different types.
     */
    @Test
    public void schemeAppend_variousCdrs() throws Exception
    {
        expectFco(
                "(append '(a) 1)",
                "(a . 1)" );
        expectFco(
                "(append '(a) 'b)",
                "(a . b)" );
    }

    /**
     * p42
     * In-line with Chez-Scheme.
     */
    @Test
    public void schemeAppend_trailingNil() throws Exception
    {
        expectFco(
                "(append '() '(1) 2)",
                "(1 . 2)" );
        expectFco(
                "(append '() '(1) '(2) 3)",
                "(1 2 . 3)" );
    }

    /**
     * p42
     * In-line with Chez-Scheme.
     */
    @Test
    public void schemeAppend_trailingNilError() throws Exception
    {
        expectError(
                "(append '() 1 2)",
                Code.TYPE_ERROR );
    }

    /**
     * p42
     */
    @Test
    public void schemeReverse_1() throws Exception
    {
        expectFco(
            "(reverse '(a b c))",
            "(c b a)" );
    }

    /**
     * p42
     */
    @Test
    public void schemeReverse_2() throws Exception
    {
        expectFco(
            "(reverse '(a (b c) d (e (f))))",
            "((e (f)) d (b c) a)" );
    }

    /**
     * Reverse empty list.
     */
    @Test
    public void schemeReverse_x1() throws Exception
    {
        expectFco(
            "(reverse '())",
            Cons.NIL );
    }

    /**
     * Single element list.
     */
    @Test
    public void schemeReverse_x2() throws Exception
    {
        expectFco(
            "(reverse '(313))",
            "(313)" );
    }

    /**
     *
     */
    @Test
    public void schemeList_Tail_x1() throws Exception
    {
        expectFco(
           "(list-tail '(a b c d) 2)",
           "(c d)" );
    }

    /**
     *
     */
    @Test
    public void schemeList_Tail_x2() throws Exception
    {
        expectFco(
              "(list-tail '(a b c d) 0)",
              "(a b c d)" );
    }

    /**
     *
     */
    @Test
    public void schemeList_Tail_x3() throws Exception
    {
        expectFco(
             "(list-tail '(a b c d) 4)",
             Cons.NIL );
    }

    /**
     * p43
     */
    @Test
    public void schemeList_Ref_1() throws Exception
    {
        expectFco( "(list-ref '(a b c d) 2)",
                "c" );
    }

    /**
     * p43
     */
    @Disabled
    @Test
    public void schemeList_Ref_2() throws Exception
    {
        expectFco( """
                (equal?
                'c
                (list-ref '(a b c d) (exact (round 1.8)))
                )
                """,
                bTrue );
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
        expectFco( """
                (define e '((a 1)(b 2)(c 3)))
                (assq 'a e)
                """,
                "(a 1)" );
    }

    /**
     * p43
     */
    @Test
    public void assq_2() throws Exception
    {
        expectFco( """
                (define e '((a 1)(b 2)(c 3)))
                (assq 'b e)
                """,
                "(b 2)" );
    }

    /**
     * p43
     */
    @Test
    public void assq_3() throws Exception
    {
        expectFco( """
                (define e '((a 1)(b 2)(c 3)))
                (assq 'd e)
                """,
                bFalse );
    }

    /**
     * p43
     */
    @Test
    public void assv_1() throws Exception
    {
        expectFco(
                "(assv 5 '((2 3)(5 7) (11 13)))",
                "(5 7)" );
    }

    /**
     * p43
     */
    @Test
    public void assoc_1() throws Exception
    {
        expectFco(
                "(assoc (list 'a) '(((a)) ((b)) ((c))))",
                "((a))" );
    }

    /**
     * p43
     */
    @Test
    public void assoc_2() throws Exception
    {
        expectFco(
                "(assoc 2.0 '((1 1) (2 4) (3 9)) =)",
                "(2 4)" );
    }

    /**
     * p43
     */
    @Test
    public void list_copy() throws Exception
    {
        expectError(
"""
        (define a '(1 8 2 8))
        (set-car! a '3)
""",
        Code.CANNOT_MODIFY_CONSTANT );

        expectFco(
"""
        (define a '(1 8 2 8))
        (define b (list-copy a))
        (set-car! b '3)
        b
""",
        "(3 8 2 8)" );
    }

    /**
     * p43
     */
    @Test
    public void list_copy_err_type() throws Exception
    {
        expectError(
"""
        (list-copy 5)
""",
        Code.TYPE_ERROR );
    }

}
