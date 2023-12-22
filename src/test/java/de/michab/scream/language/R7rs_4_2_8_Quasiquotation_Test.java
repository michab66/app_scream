/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.language;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;

/**
 * r7rs 4.2.7 p20 Quasiquotation
 *
 * https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-6.html#TAG:__tex2page_sec_4.2.8
 */
public class R7rs_4_2_8_Quasiquotation_Test extends ScreamBaseTest
{
    /**
     * p20
     */
    @Test
    public void r7rs_20_1() throws Exception
    {
        expectFco(
                "`(list ,(+ 1 2) 4)",
                "(list 3 4)" );
    }

    /**
     * p20
     */
    @Test
    public void r7rs_20_2() throws Exception
    {
        expectFco(
                "(let ((name 'a)) `(list ,name ',name))",
                "(list a (quote a))" );
    }

    /**
     * p20
     */
    @Test
    public void r7rs_20_3() throws Exception
    {
        expectFco(
                "`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)",
                "(a 3 4 5 6 b)" );
    }

    /**
     * p20
     */
    @Test
    public void r7rs_20_4() throws Exception
    {
        expectFco(
                "`(( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))",
                "((foo 7) . cons)" );
    }

    /**
     * p20
     */
    @Test
    public void r7rs_20_5() throws Exception
    {
        expectFco(
                "`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)",
                "#(10 5 2 4 3 8)" );
    }

    /**
     * p20
     */
    @Disabled( "Implement @")
    @Test
    public void r7rs_20_6() throws Exception
    {
        expectFco(
"""
                (let ((foo '(foo bar)) (@baz 'baz))
                    `(list ,@foo , @baz))
""" ,
               parse( "(list foo bar baz)" ) );
    }
}
