/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.language;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Bool;

/**
 * r7rs 6.3 Booleans.
 *
 * @author MICBINZ
 */
public class R7rs_6_3_Booleans_Test extends ScreamBaseTest
{
    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_1() throws Exception
    {
        expectFco( "#t", bTrue );
    }

    @Test
    public void r7rs_literal_true() throws Exception
    {
        expectFco( "#true", bTrue );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_2() throws Exception
    {
        expectFco( "#f", Bool.F );
    }

    @Test
    public void r7rs_literal_false() throws Exception
    {
        expectFco( "#false", bFalse );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_3() throws Exception
    {
        expectFco( "'#f", Bool.F );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_not_1() throws Exception
    {
        expectFco( "(not #t)", Bool.F );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_not_2() throws Exception
    {
        expectFco( "(not 3)", Bool.F );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_not_3() throws Exception
    {
        expectFco( "(not (list 3))", Bool.F );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_not_4() throws Exception
    {
        expectFco( "(not #f)", Bool.T );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_not_5() throws Exception
    {
        expectFco( "(not '())", Bool.F );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_not_6() throws Exception
    {
        expectFco( "(not (list))", Bool.F );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_not_7() throws Exception
    {
        expectFco( "(not 'nil)", Bool.F );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_booleanq_1() throws Exception
    {
        expectFco( "(boolean? #f)", Bool.T );
        expectFco( "(boolean? #t)", Bool.T );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_booleanq_2() throws Exception
    {
        expectFco( "(boolean? 0)", Bool.F );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_booleanq_3() throws Exception
    {
        expectFco(  "(boolean? '())", Bool.F );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_boolean_eq_4() throws Exception
    {
        expectError( "(boolean=?)", Code.WRONG_NUMBER_OF_ARGUMENTS );
    }

    @Test
    public void boolean_eq_5() throws Exception
    {
        expectFco( "(boolean=? #t)", Bool.T );
    }

    @Test
    public void boolean_eq_6() throws Exception
    {
        expectFco( "(boolean=? #t #t #t #t #t #t #t #t)", Bool.T );
    }

    @Test
    public void boolean_eq_7() throws Exception
    {
        expectFco( "(boolean=? #f #f #f #f #f #f)", Bool.T );
    }

    @Test
    public void boolean_eq_8() throws Exception
    {
        expectFco( "(boolean=? #t #t #f #t #t #t)", Bool.F );
    }

    @Test
    public void boolean_eq_9() throws Exception
    {
        expectError( "(boolean=? #t #t #t #t #t 1)", Code.TYPE_ERROR );
    }
}
