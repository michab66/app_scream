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
        var t = makeTester();

        t.expectFco( "#t", Bool.T );
        t.expectFco( "#true", Bool.T );
        t.expectFco( "#T", Bool.T );
        t.expectFco( "#TRUE", Bool.T );

        t.expectFco( "#f", Bool.F );
        t.expectFco( "#false", Bool.F );
        t.expectFco( "#F", Bool.F );
        t.expectFco( "#FALSE", Bool.F );

        t.expectFco( "'#f", Bool.F );
        t.expectFco( "'#t", Bool.T );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_not_1() throws Exception
    {
        var t = makeTester();

        t.expectFco( "(not #t)", Bool.F );
        t.expectFco( "(not 3)", Bool.F );
        t.expectFco( "(not (list 3))", Bool.F );
        t.expectFco( "(not #f)", Bool.T );
        t.expectFco( "(not '())", Bool.F );
        t.expectFco( "(not (list))", Bool.F );
        t.expectFco( "(not 'nil)", Bool.F );
    }

    /**
     * r7rs 6.3 p40
     */
    @Test
    public void r7rs_booleanq_1() throws Exception
    {
        var t = makeTester();

        t.expectFco( "(boolean? #f)", Bool.T );
        t.expectFco( "(boolean? #t)", Bool.T );
        t.expectFco( "(boolean? 0)", Bool.F );
        t.expectFco(  "(boolean? '())", Bool.F );
        t.expectError( "(boolean=?)", Code.WRONG_NUMBER_OF_ARGUMENTS );
    }

    @Test
    public void boolean_eq_5() throws Exception
    {
        var t = makeTester();

        t.expectFco( "(boolean=? #t)", Bool.T );
        t.expectFco( "(boolean=? #t #t #t #t #t #t)", Bool.T );
        t.expectFco( "(boolean=? #f #f #f #f #f #f)", Bool.T );
        t.expectFco( "(boolean=? #t #t #FALSE #t #t #t)", Bool.F );
        t.expectError( "(boolean=? #t #t #t #t #t 1)", Code.TYPE_ERROR );
        t.expectError( "(boolean=? #t #t #t #t #t '())", Code.TYPE_ERROR );
        t.expectError( "(boolean=? '())", Code.TYPE_ERROR );
    }
}
