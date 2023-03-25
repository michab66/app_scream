/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.language;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamException.Code;
import de.michab.scream.fcos.SchemeBoolean;

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
        expectFco( "#t", SchemeBoolean.T );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_2() throws Exception
    {
        expectFco( "#f", SchemeBoolean.F );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_3() throws Exception
    {
        expectFco( "'#f", SchemeBoolean.F );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_not_1() throws Exception
    {
        expectFco( "(not #t)", SchemeBoolean.F );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_not_2() throws Exception
    {
        expectFco( "(not 3)", SchemeBoolean.F );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_not_3() throws Exception
    {
        expectFco( "(not (list 3))", SchemeBoolean.F );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_not_4() throws Exception
    {
        expectFco( "(not #f)", SchemeBoolean.T );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_not_5() throws Exception
    {
        expectFco( "(not '())", SchemeBoolean.F );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_not_6() throws Exception
    {
        expectFco( "(not (list))", SchemeBoolean.F );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_not_7() throws Exception
    {
        expectFco( "(not 'nil)", SchemeBoolean.F );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_booleanq_1() throws Exception
    {
        expectFco( "(boolean? #f)", SchemeBoolean.T );
        expectFco( "(boolean? #t)", SchemeBoolean.T );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_booleanq_2() throws Exception
    {
        expectFco( "(boolean? 0)", SchemeBoolean.F );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_booleanq_3() throws Exception
    {
        expectFco(  "(boolean? '())", SchemeBoolean.F );
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
        expectFco( "(boolean=? #t)", SchemeBoolean.T );
    }

    @Test
    public void boolean_eq_6() throws Exception
    {
        expectFco( "(boolean=? #t #t #t #t #t #t #t #t)", SchemeBoolean.T );
    }

    @Test
    public void boolean_eq_7() throws Exception
    {
        expectFco( "(boolean=? #f #f #f #f #f #f)", SchemeBoolean.T );
    }

    @Test
    public void boolean_eq_8() throws Exception
    {
        expectFco( "(boolean=? #t #t #f #t #t #t)", SchemeBoolean.F );
    }

    @Test
    public void boolean_eq_9() throws Exception
    {
        expectError( "(boolean=? #t #t #t #t #t 1)", Code.TYPE_ERROR );
    }
}
