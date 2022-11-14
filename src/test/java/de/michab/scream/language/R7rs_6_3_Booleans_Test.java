package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import de.michab.scream.SchemeBoolean;
import de.michab.scream.ScreamBaseTest;

/**
 * r7rs 6.3 Booleans.
 *
 * @author MICBINZ
 */
public class R7rs_6_3_Booleans_Test extends ScreamBaseTest
{
    /**
     * p13
     */
    @Test
    public void basic_1() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            #t
            """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p13
     */
    @Test
    public void basic_2() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            #f
            """ );
        assertEquals( SchemeBoolean.F, result );
    }

    /**
     * p13
     */
    @Test
    public void basic_3() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            '#f
            """ );
        assertEquals( SchemeBoolean.F, result );
    }

    /**
     * p13
     */
    @Test
    public void not_1() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (not #t)
            """ );
        assertEquals( SchemeBoolean.F, result );
    }

    /**
     * p13
     */
    @Test
    public void not_2() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (not 3)
            """ );
        assertEquals( SchemeBoolean.F, result );
    }

    /**
     * p13
     */
    @Test
    public void not_3() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (not (list 3))
            """ );
        assertEquals( SchemeBoolean.F, result );
    }

    /**
     * p13
     */
    @Test
    public void not_4() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (not #f)
            """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p13
     */
    @Test
    public void not_5() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (not '())
            """ );
        assertEquals( SchemeBoolean.F, result );
    }

    /**
     * p13
     */
    @Test
    public void not_5b() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (not ())
            """ );
        assertEquals( SchemeBoolean.F, result );
    }

    /**
     * p13
     */
    @Test
    public void not_6() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (not (list))
            """ );
        assertEquals( SchemeBoolean.F, result );
    }

    /**
     * p13
     */
    @Test
    public void not_7() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (not 'nil)
            """ );
        assertEquals( SchemeBoolean.F, result );
    }

    /**
     * p13
     */
    @Test
    public void booleanq_1() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (boolean? #f)
            """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p13
     */
    @Test
    public void booleanq_2() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (boolean? 0)
            """ );
        assertEquals( SchemeBoolean.F, result );
    }

    /**
     * p13
     */
    @Test
    public void booleanq_3() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (boolean? '())
            """ );
        assertEquals( SchemeBoolean.F, result );
    }

    /**
     * p13
     */
    @Test
    @Disabled( "not implemented" )
    public void boolean_eq_3() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (boolean=? #t #t #t)
            """ );
        assertEquals( SchemeBoolean.T, result );
    }

}
