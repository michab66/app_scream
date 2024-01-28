/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.SchemeDouble;
import de.michab.scream.fcos.SchemeInteger;

public class R7rs_6_2_6_Numerical_operations_Test extends ScreamBaseTest
{
    /**
     * r7rs exact/inexact 6.2.6 p35
     */
    @Test
    public void exact_inexact() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(exact? 3.0)",
                bFalse );
        t.expectFco(
                "(exact? #e3.0)",
                bTrue );
        t.expectFco(
                "(inexact? 3.)",
                bTrue );
    }

    /**
     * r7rs exact/inexact 6.2.6 p35
     */
    @Test
    public void exact_inexact_integer() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(exact-integer? 32)",
                bTrue );
        t.expectFco(
                "(exact-integer? 32.0)",
                bFalse );
    }

    /**
     * r7rs truncate 6.2.6 p37
     */
    @Test
    public void truncate_1() throws Exception
    {
        expectFco(
                "(truncate -4.3)",
                d( -4.0 ) );
    }

    /**
     * r7rs truncate 6.2.6 p37
     */
    @Test
    public void truncate_2() throws Exception
    {
        expectFco(
                "(truncate 3.5)",
                d( 3.0 ) );
    }

    @Test
    public void round() throws Exception
    {
        var ts = makeTester();

        ts.expectFco( "(round -4.3)", d(-4.0) );
        ts.expectFco( "(exact? (round -4.3))", bFalse );
        ts.expectFco( "(round 3.5)", d(4.0) );
        ts.expectFco( "(round 7)", i(7) );
        ts.expectFco( "(exact? (round 7))", bTrue );
        ts.expectError( "(round 'me)", Code.TYPE_ERROR );
    }

    @Test
    public void sqrt() throws Exception
    {
        final var SQRT2 = Math.sqrt( 2.0 );

        expectFco(
                "(sqrt 2)",
                d( SQRT2 ) );
    }
    @Test
    public void sqrt_2() throws Exception
    {
        final var SQRT2 = Math.sqrt( 2.0 );

        expectFco(
                "(sqrt 2.0)",
                d( SQRT2 ) );
    }
    @Test
    public void sqrt_3() throws Exception
    {
        expectFco(
                "(integer? (sqrt 4))",
                bTrue );
    }

    @Test
    public void exact_integer_sqrt() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(exact-integer-sqrt 4)",
                "(2 0)" );
        t.expectFco(
                "(exact-integer-sqrt 5)",
                "(2 1)" );
        t.expectFco(
                "(exact-integer-sqrt 32)",
                "(5 7)" );
    }

    @Test
    public void inexact() throws Exception
    {
        var t = makeTester();

        assertInstanceOf(
                SchemeDouble.class,
                t.execute( "(inexact 3.1415)" ) );

        // Workaround for #261.
        t.expectFco(
                "(inexact? (inexact 3))",
                bTrue );
        t.expectFco(
                "(inexact? (inexact 3.0))",
                bTrue );
    }

    @Test
    public void exact() throws Exception
    {
        var t = makeTester();

        assertInstanceOf(
                SchemeInteger.class,
                t.execute( "(exact 313)" ) );

        // Workaround for #261, better is "(exact 313) -> 313".
        t.expectFco(
                "(exact? (exact 313))",
                bTrue );
        t.expectFco(
                "(exact? (exact 3.0))",
                bTrue );
        t.expectError(
                "(exact 3.13)",
                Code.ILLEGAL_ARGUMENT );
    }
}
