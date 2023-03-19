/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.SchemeBoolean;

/**
 * r7rs 4.2.1 Conditionals.
 *
 * @author MICBINZ
 */
public class R7rs_4_2_1_Conditionals_Test extends ScreamBaseTest
{
    @Test
    public void ifTest() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (if #t 313 0)
                """ );
        assertEquals( i313, result );
    }

    @Test
    public void ifTestNoElse() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (if #f 313)
                """ );
        assertEquals( SchemeBoolean.F, result );
    }

    @Test
    public void _ifTest() throws Exception
    {
        expectFco(
                """
                (if #t 313 0)
                """,
                i313 );
    }
    /**
     * p14
     */
    @Test
    public void cond_1() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (cond ((> 3 2) 'greater)
                  ((< 3 2) 'less))
            """ );
        assertEquals( s("greater"), result );
    }

    /**
     * p14
     */
    @Test
    public void cond_2() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (cond ((> 3 3) 'greater)
                  ((< 3 3) 'less)
                  (else 'equal))
            """ );
        assertEquals( s("equal"), result );
    }

    /**
     * p14
     */
    @Disabled( "not implemented." )
    @Test
    public void cond_3() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (cond ((assv 'b '((a 1) (b 2))) => cadr)
                  (else #f))
            """ );
        assertEquals( i(2), result );
    }

    /**
     * p15
     */
    @Test
    public void case_1() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (case (* 2 3)
             ((2 3 5 7) 'prime)
             ((1 4 6 8 ) 'composite))
            """ );
        assertEquals( s("composite"), result );
    }

    /**
     * p15
     */
    @Test
    public void case_2() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (case (car '(c d))
             ((a) 'a)
             ((b) 'b))
            """ );
        assertEquals( Cons.NIL, result );
    }

    /**
     * p15
     */
    @Test
    @Disabled("not implemented")
    public void case_3() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (case (car '(c d))
             ((a e i o u) 'vowel)
             ((w y) 'semivowel)
             (else => (lambda (x) x)))
            """ );
        assertEquals( s("c"), result );
    }

    /**
     * p15
     */
    @Test
    public void and_1() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (and (= 2 2) (> 2 1))
            """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p15
     */
    @Test
    public void and_2() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (and (= 2 2) (< 2 1))
            """ );
        assertEquals( SchemeBoolean.F, result );
    }

    /**
     * p15
     */
    @Test
    public void and_3() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (and 1 2 'c '(f g))
            """ );
        assertEqualq( parse( "(f g)" ), result );
    }

    /**
     * p15
     */
    @Test
    public void and_4() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (and)
            """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p15
     */
    @Test
    public void or_1() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (or (= 2 2) (> 2 1))
            """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p15
     */
    @Test
    public void or_2() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (or (= 2 2) (< 2 1))
            """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p15
     */
    @Test
    public void or_3() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (or #f #f #f)
            """ );
        assertEquals( SchemeBoolean.F, result );
    }

    /**
     * p15
     */
    @Test
    public void or_4() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (or (memq 'b '(a b c))
                (/ 3 0))
            """ );
        assertEqualq( parse( "(b c)" ), result );
    }

    /**
     * p15
     */
    @Test
    public void or_5() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (or)
            """ );
        assertEqualq( SchemeBoolean.F, result );
    }

}
