/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamException.Code;
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
        var result = scriptEngine().evalFco(
            """
            (equal?
              '(a b c d e)
              '(a . (b . (c . (d . (e . ())))))
            )
            """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p41
     */
    @Test
    public void schemeListNotation_2() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (equal?
              '(a b c . d)
              '(a . (b . (c . d)))
            )
            """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p41
     */
    @Test
    public void schemePair_1() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (pair? '(a . b))
                """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p41
     */
    @Test
    public void schemePair_2() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (pair? '(a b c))
                """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p41
     */
    @Test
    public void schemePair_3() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (pair? '())
                """ );
        assertEquals( SchemeBoolean.F, result );
    }

    /**
     * p41
     */
    @Test
    public void schemePair_4() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (pair? '#(a b))
                """ );
        assertEquals( SchemeBoolean.F, result );
    }

    /**
     * p41
     */
    @Disabled( "make-list not implemented." )
    @Test
    public void schemeCons_1() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (equal?
                        '(a)
                        (cons 'a '())
                )
                """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p41
     */
    @Disabled( "make-list not implemented." )
    @Test
    public void schemeCons_2() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (equal?
                        '((a) b c d)
                        (cons '(a) '(b c d))
                )
                """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p41
     */
    @Disabled( "make-list not implemented." )
    @Test
    public void schemeCons_3() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (equal?
                        '("a" b c)
                        (cons "a" '(b c))
                )
                """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p41
     */
    @Disabled( "make-list not implemented." )
    @Test
    public void schemeCons_4() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (equal?
                        '(a . 3)
                        (cons 'a 3)
                )
                """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p41
     */
    @Disabled( "make-list not implemented." )
    @Test
    public void schemeCons_5() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (equal?
                        '((a b) . c)
                        (cons '(a b) 'c)
                )
                """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p42
     */
    @Test
    public void schemeNull_x1() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (null? '(a b c))
                """ );
        assertEquals( SchemeBoolean.F, result );
    }

    /**
     * p42
     */
    @Test
    public void schemeNull_x2() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (null? '())
                """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p42
     */
    @Test
    public void schemeListQ_1() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (list? '(a b c))
                """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p42
     */
    @Test
    public void schemeListQ_2() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (list? '())
                """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p42
     */
    @Test
    public void schemeListQ_3() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (list? '(a . c))
                """ );
        assertEquals( SchemeBoolean.F, result );
    }

    /**
     * p42
     */
    @Test
    public void schemeListQ_4() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (let ((x (list 'a)))
                  (set-cdr! x x)
                  (list? x))
                """ );
        assertEquals( SchemeBoolean.F, result );
    }

    /**
     * p42
     */
    @Disabled( "make-list not implemented." )
    @Test
    public void schemeMake_List_1() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (equal?
                        '(3 3)
                        (make-list 2 3)
                )
                """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p42
     */
    @Disabled( "make-list not implemented." )
    @Test
    public void schemeMake_List_x1() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (equal?
                        '(() ())
                        (make-list 2)
                )
                """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p42
     */
    @Test
    public void schemeList_1() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (equal?
                        '(a 7 c)
                        (list 'a (+ 3 4) 'c)
                )
                """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p42
     */
    @Test
    public void schemeList_2() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (equal?
                        '()
                        (list)
                )
                """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p42
     */
    @Test
    public void schemeLength_1() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (length '(a b c))
                """ );
        assertEquals( i(3), result );
    }

    /**
     * p42
     */
    @Test
    public void schemeLength_2() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (length '(a (b) (c d e)))
                """ );
        assertEquals( i(3), result );
    }

    /**
     * p42
     */
    @Test
    public void schemeLength_3() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (length '())
                """ );
        assertEquals( i(0), result );
    }

    @Test
    public void schemeLength_improperFail() throws Exception
    {
        try
        {
            scriptEngine().evalFco(
                """
                (length '(a b . c))
                """ );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.EXPECTED_PROPER_LIST, rx.getCode() );
        }
    }

    /**
     * p42
     */
    @Test
    public void schemeAppend_1() throws Exception
    {
        expectFco(
                "(append '(x) '(y))",
                parse( "(x y)" ) );
    }

    /**
     * p42
     */
    @Test
    public void schemeAppend_2() throws Exception
    {
        expectFco(
                "(append '(a) '(b c d))",
                parse( "(a b c d)" ));
    }

    /**
     * p42
     */
    @Test
    public void schemeAppend_3() throws Exception
    {
        expectFco(
                "(append '(a (b)) '((c)))",
                parse( "(a (b) (c))" ));
    }

    /**
     * p42
     */
    @Test
    public void schemeAppend_4() throws Exception
    {
        expectFco(
                "(append '(a b) '(c . d))",
                parse( "(a b c . d)" ));
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
                s("a") );
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
                parse( "(a . 1)" ) );
        expectFco(
                "(append '(a) 'b)",
                parse( "(a . b)" ) );
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
                parse( "(1 . 2)" ) );
        expectFco(
                "(append '() '(1) '(2) 3)",
                parse( "(1 2 . 3)" ) );
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
        var result = scriptEngine().evalFco(
            """
            (equal?
                    '(c b a)
                    (reverse '(a b c))
            )
            """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p42
     */
    @Test
    public void schemeReverse_2() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (equal?
                    '((e (f)) d (b c) a)
                    (reverse '(a (b c) d (e (f))))
            )
            """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * Reverse empty list.
     */
    @Test
    public void schemeReverse_x1() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (equal?
                    '()
                    (reverse '())
            )
            """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * Single element list.
     */
    @Test
    public void schemeReverse_x2() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (equal?
                    '(313)
                    (reverse '(313))
            )
            """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     *
     */
    @Test
    public void schemeList_Tail_x1() throws Exception
    {
        var result = scriptEngine().evalFco(
           """
           (equal?
                   '(c d)
                   (list-tail '(a b c d) 2)
           )
           """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     *
     */
    @Test
    public void schemeList_Tail_x2() throws Exception
    {
        var result = scriptEngine().evalFco(
              """
              (equal?
                '(a b c d)
                (list-tail '(a b c d) 0)
              )
              """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     *
     */
    @Test
    public void schemeList_Tail_x3() throws Exception
    {
        var result = scriptEngine().evalFco(
             """
             (equal?
               '()
               (list-tail '(a b c d) 4)
             )
             """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p43
     */
    @Test
    public void schemeList_Ref_1() throws Exception
    {
        var result = scriptEngine().evalFco( """
                (equal?
                'c
                (list-ref '(a b c d) 2)
                )
                """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p43
     */
    @Disabled
    @Test
    public void schemeList_Ref_2() throws Exception
    {
        var result = scriptEngine().evalFco( """
                (equal?
                'c
                (list-ref '(a b c d) (exact (round 1.8)))
                )
                """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p43
     */
    @Test
    public void memq_1() throws Exception
    {
        var result = scriptEngine().evalFco( """
                (memq 'a '(a b c))
                """ );
        assertEqualq( parse( "(a b c)" ), result );
    }

    /**
     * p43
     */
    @Test
    public void memq_2() throws Exception
    {
        var result = scriptEngine().evalFco( """
                (memq 'b '(a b c))
                """ );
        assertEqualq( parse( "(b c)" ), result );
    }

    /**
     * p43
     */
    @Test
    public void memq_3() throws Exception
    {
        var result = scriptEngine().evalFco( """
                (memq 'a '(b c d))
                """ );
        assertEqualq( SchemeBoolean.F, result );
    }

    /**
     * p43
     */
    @Test
    public void memq_4() throws Exception
    {
        var result = scriptEngine().evalFco( """
                (memq (list 'a) '(b (a) c))
                """ );
        assertEqualq( SchemeBoolean.F, result );
    }

    /**
     * p43
     */
    @Test
    public void member_1() throws Exception
    {
        var result = scriptEngine().evalFco( """
                (member (list 'a)
                '(b (a) c))
                """ );
        assertEqualq( parse( "((a) c)" ), result );
    }

    /**
     * p43
     */
    @Test
    @Disabled("not implemented")
    public void member_2() throws Exception
    {
        var result = scriptEngine().evalFco( """
                (member "B"
                 '("a", "b", "c")
                 string-ci=?)
                """ );
        assertEqualq( readSingleExpression(
                """
                 ("b", "c")
                """,
                Cons.class ), result );
    }

    /**
     * p43
     */
    @Test
    public void memq_5() throws Exception
    {
        var result = scriptEngine().evalFco( """
                (memq 101 '(100 101 102))
                """ );
        assertEqualq( SchemeBoolean.F, result );
    }

    /**
     * p43
     */
    @Test
    public void memv_1() throws Exception
    {
        var result = scriptEngine().evalFco( """
                (memv 101 '(100 101 102))
                """ );
        assertEqualq( parse( "(101 102)" ), result );
    }

}
