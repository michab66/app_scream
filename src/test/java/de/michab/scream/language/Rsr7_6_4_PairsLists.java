package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import javax.script.ScriptException;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.SchemeBoolean;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamException.Code;

/**
 * rsr7 6.4 Pairs and lists.
 *
 * @author MICBINZ
 */
public class Rsr7_6_4_PairsLists extends ScreamBaseTest
{
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
        catch ( ScriptException x )
        {
            RuntimeX rx = (RuntimeX)x.getCause();
            assertEquals( Code.EXPECTED_PROPER_LIST, rx.getCode() );
        }
    }

    /**
     * p42
     */
    @Test
    public void schemeAppend_1() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (equal?
                    '(x y)
                    (append '(x) '(y)))
            """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p42
     */
    @Test
    public void schemeAppend_2() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (equal?
                    '(a b c d)
                    (append '(a) '(b c d)))
            """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p42
     */
    @Test
    public void schemeAppend_3() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (equal?
                    '(a (b) (c))
                    (append '(a (b)) '((c)))
            )
            """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p42
     */
    @Test
    public void schemeAppend_4() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (equal?
                    '(a b c . d)
                    (append '(a b) '(c . d))
            )
            """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p42
     */
    @Test
    public void schemeAppend_5() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (equal?
                    'a
                    (append '() 'a)
            )
            """ );
        assertEquals( SchemeBoolean.T, result );
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
     * p42
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
     * p42
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

}
