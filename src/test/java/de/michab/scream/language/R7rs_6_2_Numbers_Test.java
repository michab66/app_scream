/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamEvaluator;
import de.michab.scream.fcos.SchemeBoolean;
import de.michab.scream.fcos.SchemeDouble;
import de.michab.scream.fcos.SchemeInteger;

public class R7rs_6_2_Numbers_Test extends ScreamBaseTest
{
    /**
     * r7rs truncate 6.2.6 p37
     */
    @Test
    public void truncate() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(truncate -4.3)",
                d( -4.0 ) );
        t.expectFco(
                "(truncate 3.5)",
                d( 3.0 ) );
    }

    @Test
    public void round() throws Exception
    {
        var ts = makeTester();

        ts.expectFco(
                "(round -4.3)",
                d(-4.0) );
        ts.expectFco(
                "(exact? (round -4.3))",
                bFalse );
        ts.expectFco(
                "(round 3.5)",
                d(4.0) );
        ts.expectFco(
                "(round 7)",
                i(7) );
        ts.expectFco(
                "(exact? (round 7))",
                bTrue );
        ts.expectError(
                "(round 'me)", Code.TYPE_ERROR );
    }

    @Test
    public void sqrt() throws Exception
    {
        var t = makeTester();

        final var SQRT2 = Math.sqrt( 2.0 );

        t.expectFco(
                "(sqrt 2)",
                d( SQRT2 ) );
        t.expectFco(
                "(sqrt 2.0)",
                d( SQRT2 ) );
        t.expectFco(
                "(integer? (sqrt 4))",
                bTrue );
    }

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

    @Test
    public void mathEquals_t() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (= 313 313)
                """ );
        assertEquals( SchemeBoolean.T, result );
    }

    @Test
    public void mathEquals_f() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (= 121 313)
                """ );
        assertEquals( SchemeBoolean.F, result );
    }

    @Test
    public void _equalsCompile() throws Exception
    {
        expectFco( "(= 121 121)", SchemeBoolean.T );
    }

    @Test
    public void _equalsCompile2() throws Exception
    {
        expectFco( "(define i 121)(= i 121)", SchemeBoolean.T );
    }

    @Test
    public void addition_err_1() throws Exception
    {
        expectError( "(+ 1 'i)", Code.TYPE_ERROR );
    }

//    ;;
//    ;; Test =
//    ;;
//    (%positive-test sourcefile 1
//      (= 1 1.0 (- 3 2) (* .5 2))
//      #t)
//    (%positive-test sourcefile 2
//      (= 1 1.0 (- 3 2) (* .5 3))
//      #f)
//
//    ;;
//    ;; Test <=>
//    ;;
//    (%positive-test sourcefile 3
//      (<= 1 1.0 1.5 2.0 2)
//      #t)
//    (%positive-test sourcefile 4
//      (<= 1 1.5 1)
//      #f)
//    (%positive-test sourcefile 5
//      (< 1 1.5 2)
//      #t)
//    (%positive-test sourcefile 6
//      (< 1 1.0 1.5 2.0 2)
//      #f)
//
//    (%positive-test sourcefile 7
//      (> 1 0.0 -1)
//      #t)
//    (%positive-test sourcefile 8
//      (> 1 0.0 1)
//      #f)
//    (%positive-test sourcefile 9
//      (>= 1 1.0 1 0.0 0 0.0 -1 -1.0 -1)
//      #t)
//    (%positive-test sourcefile 10
//      (>= 1 1.0 1.5 2.0 2)
//      #f)
//
//    ;;
//    ;;
//    ;;
//    (%positive-test sourcefile 11
//      (zero? 0)
//      #t)
//    (%positive-test sourcefile 12
//      (zero? 0.0)
//      #t)
//    (%positive-test sourcefile 13
//      (zero? 1)
//      #f)
//    (%positive-test sourcefile 14
//      (zero? -1.0)
//      #f)
//
//    ;;
//    ;;
//    ;;
//    (%positive-test sourcefile 15
//      (positive? 0)
//      #f)
//    (%positive-test sourcefile 16
//      (positive? -1)
//      #f)
//    (%positive-test sourcefile 17
//      (positive? 1)
//      #t)
//    (%positive-test sourcefile 18
//      (negative? 0)
//      #f)
//    (%positive-test sourcefile 19
//      (negative? -1)
//      #t)
//    (%positive-test sourcefile 20
//      (negative? 1)
//      #f)
//
//    ;;
//    ;;
//    ;;
//    (%positive-test sourcefile 21
//      (odd? 1)
//      #t)
//    (%positive-test sourcefile 22
//      (odd? 2)
//      #f)
//    (%positive-test sourcefile 23
//      (even? 1)
//      #f)
//    (%positive-test sourcefile 24
//      (even? 2)
//      #t)
//    (%positive-test sourcefile 25
//      (even? 0)
//      #t)
//
//    ;;;
//    (%positive-test sourcefile 26
//      (max 3 4 5)
//      5)
//    (%positive-test sourcefile 27
//      (min 3 4 5)
//      3)
//
//    ;; +*
//    (%positive-test sourcefile 28
//      (+ 3 4)
//      7)
//    (%positive-test sourcefile 29
//      (+ 3)
//      3)
//    (%positive-test sourcefile 30
//      (+)
//      0)
//
//    (%positive-test sourcefile 31
//      (* 3 4)
//      12)
//    (%positive-test sourcefile 32
//      (* 4)
//      4)
//    (%positive-test sourcefile 33
//      (*)
//      1)
//
//    ;; -/
//    (%positive-test sourcefile 34
//      (- 3 4)
//      -1)
//    (%positive-test sourcefile 35
//      (- 3 4 5)
//      -6)
//    (%positive-test sourcefile 36
//      (- 3)
//      -3)
//    (%positive-test sourcefile 37
//      (/ 3.0 4 5) ; TODO
//      0.15)
//    (%positive-test sourcefile 38
//      (/ 3.0)
//      0.3333333333333333)
//
//    ;;
//    (%positive-test sourcefile 39
//      (abs -7)
//      7)
//
//    ;;
//    (%positive-test sourcefile 40
//      (modulo 13 4)
//      1)
//    (%positive-test sourcefile 41
//      (remainder 13 4)
//      1)
//    (%positive-test sourcefile 42
//      (modulo -13 4)
//      3)
//    (%positive-test sourcefile 43
//      (remainder -13 4)
//      -1)
//    (%positive-test sourcefile 44
//      (modulo 13 -4)
//      -3)
//    (%positive-test sourcefile 45
//      (remainder 13 -4)
//      1)
//    (%positive-test sourcefile 46
//      (modulo -13 -4)
//      -1)
//    (%positive-test sourcefile 47
//      (remainder -13 -4)
//      -1)
//
//    ;;
//    (%positive-test sourcefile 48
//      (gcd 32 -36)
//      4)
//    (%positive-test sourcefile 49
//      (gcd)
//      0)
//    (%positive-test sourcefile 50
//      (lcm 32 -36)
//      288)
//    (%positive-test sourcefile 51
//      (lcm)
//      1)
//
//    ;;
//    (%positive-test sourcefile 52
//      (floor -4.3)
//      -5.0)
//    (%positive-test sourcefile 53
//      (ceiling -4.3)
//      -4.0)
//    (%positive-test sourcefile 54
//      (truncate -4.3)
//      -4.0)
//    (%positive-test sourcefile 55
//      (round -4.3)
//      -4.0)
//    (%positive-test sourcefile 56
//      (floor 3.5)
//      3.0)
//    (%positive-test sourcefile 57
//      (ceiling 3.5)
//      4.0)
//    (%positive-test sourcefile 58
//      (truncate 3.5)
//      3.0)
//    (%positive-test sourcefile 59
//      (round 3.5)
//      4.0)

}
