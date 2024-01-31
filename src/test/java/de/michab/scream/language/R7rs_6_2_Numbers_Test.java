/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.SchemeDouble;
import de.michab.scream.fcos.SchemeInteger;

public class R7rs_6_2_Numbers_Test extends ScreamBaseTest
{
    @Test
    public void mathEquals() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(= 1 1)",
                bTrue );
        t.expectFco(
                "(= 1 1 1)",
                bTrue );
        t.expectFco(
                "(= 2 1)",
                bFalse );
        t.expectFco(
                "(= 2 2 1)",
                bFalse );

        t.expectFco(
                "(= 1 1.0)",
                bTrue );
        t.expectFco(
                "(= 1 1.0 (+ -1 2))",
                bTrue );
        t.expectFco(
                "(= 1 1.1)",
                bFalse );
        t.expectFco(
                "(= 1.1 1)",
                bFalse );
        t.expectFco(
                "(= 1.9 1)",
                bFalse );
        t.expectFco(
                "(= 1 1.0 3.14)",
                bFalse );

        t.expectFco(
                "(= 1 1.0 (- 3 2) (* .5 2))",
                bTrue );
        t.expectFco(
                "(= 1 1.0 (- 3 2) (* .5 3))",
                bFalse );

        t.expectFco( "(= 1)",
                bTrue );
        t.expectFco( "(= 3.14159265)",
                bTrue );

        t.expectError( "(=)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
        t.expectError( "(= '())",
                Code.TYPE_ERROR );
    }

    @Test
    public void mathLess() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(< -1 1)",
                bTrue );
        t.expectFco(
                "(< -1 0 1 2)",
                bTrue );

        t.expectFco(
                "(< 2 2)",
                bFalse );
        t.expectFco(
                "(< 3 2)",
                bFalse );
        t.expectFco(
                "(< -1 0 0 2)",
                bFalse );

        t.expectFco(
                "(< -1 1.0)",
                bTrue );
        t.expectFco(
                "(< -1.0 1)",
                bTrue );

        t.expectFco(
                "(< -1 1.0 2)",
                bTrue );
        t.expectFco(
                "(< -1.0 1 2.0)",
                bTrue );

        t.expectFco(
                "(< 0)",
                bTrue );
        t.expectError( "(<)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
        t.expectError( "(< '())",
                Code.TYPE_ERROR );
    }

    @Test
    public void mathGreater() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(> -1 1)",
                bFalse );
        t.expectFco(
                "(> -1 0 1 2)",
                bFalse );

        t.expectFco(
                "(> 2 2)",
                bFalse );
        t.expectFco(
                "(> 3 2)",
                bTrue );
        t.expectFco(
                "(> -1 0 0 2)",
                bFalse );

        t.expectFco(
                "(> -1 1.0)",
                bFalse );
        t.expectFco(
                "(> -1.0 1)",
                bFalse );

        t.expectFco(
                "(> -1 1.0 2)",
                bFalse );
        t.expectFco(
                "(> -1.0 1 2.0)",
                bFalse );

        t.expectFco(
                "(> 0)",
                bTrue );
        t.expectError( "(>)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
        t.expectError( "(> '())",
                Code.TYPE_ERROR );
    }

    @Test
    public void mathLessEqual() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(<= -1 1)",
                bTrue );
        t.expectFco(
                "(<= -1 -1)",
                bTrue );
        t.expectFco(
                "(<= 1 0)",
                bFalse );

        t.expectFco(
                "(<= -1. 1)",
                bTrue );
        t.expectFco(
                "(<= -1. -1)",
                bTrue );
        t.expectFco(
                "(<= 1. 0)",
                bFalse );

        t.expectFco(
                "(<= -1. 1.)",
                bTrue );
        t.expectFco(
                "(<= -1. -1.)",
                bTrue );
        t.expectFco(
                "(<= 1. 0.)",
                bFalse );

        t.expectFco(
                "(<= 1 1.0 1.5 2.0 2)",
                bTrue );
        t.expectFco(
                "(<= 1 1.5 1)",
                bFalse );

        t.expectFco(
                "(<= 0)",
                bTrue );
        t.expectError( "(<=)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
        t.expectError( "(<= '())",
                Code.TYPE_ERROR );
    }

    @Test
    public void mathGreaterEqual() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(>= -1 1)",
                bFalse );
        t.expectFco(
                "(>= -1 -1)",
                bTrue );
        t.expectFco(
                "(>= 1 0)",
                bTrue );

        t.expectFco(
                "(>= -1. 1)",
                bFalse );
        t.expectFco(
                "(>= -1. -1)",
                bTrue );
        t.expectFco(
                "(>= 1. 0)",
                bTrue );

        t.expectFco(
                "(>= -1. 1.)",
                bFalse );
        t.expectFco(
                "(>= -1. -1.)",
                bTrue );
        t.expectFco(
                "(>= 1. 0.)",
                bTrue );

        t.expectFco(
                "(>= 0)",
                bTrue );
        t.expectError( "(>=)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
        t.expectError( "(>= '())",
                Code.TYPE_ERROR );
    }

    /**
     * r7rs zero? 6.2.6 p36
     */
    @Test
    public void zeroq() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(zero? 0)",
                bTrue );
        t.expectFco(
                "(zero? 0.0)",
                bTrue );
        t.expectFco(
                "(zero? 313)",
                bFalse );
        t.expectFco(
                "(zero? 3.1415)",
                bFalse );        t.expectError(
                "(zero? '())",
                Code.TYPE_ERROR );
        t.expectError(
                "(zero?)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
        t.expectError(
                "(zero? 0 0)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
    }

    /**
     * r7rs positive? 6.2.6 p36
     */
    @Test
    public void positiveq() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(positive? 0)",
                bFalse );
        t.expectFco(
                "(positive? -1)",
                bFalse );
        t.expectFco(
                "(positive? 1)",
                bTrue );

        t.expectFco(
                "(positive? 0.0)",
                bFalse );
        t.expectFco(
                "(positive? -1.)",
                bFalse );
        t.expectFco(
                "(positive? 1.)",
                bTrue );

        t.expectError(
                "(positive? '())",
                Code.TYPE_ERROR );
        t.expectError(
                "(positive?)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
        t.expectError(
                "(positive? 0 0)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
    }

    /**
     * r7rs negative? 6.2.6 p36
     */
    @Test
    public void negativeq() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(negative? 0)",
                bFalse );
        t.expectFco(
                "(negative? -1)",
                bTrue );
        t.expectFco(
                "(negative? 1)",
                bFalse );

        t.expectFco(
                "(negative? 0.0)",
                bFalse );
        t.expectFco(
                "(negative? -1.)",
                bTrue );
        t.expectFco(
                "(negative? 1.)",
                bFalse );

        t.expectError(
                "(negative? '())",
                Code.TYPE_ERROR );
        t.expectError(
                "(negative?)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
        t.expectError(
                "(negative? 0 0)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
    }

    /**
     * r7rs odd? 6.2.6 p36
     */
    @Test
    public void oddq() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(odd? -1)",
                bTrue );
        t.expectFco(
                "(odd? 0)",
                bFalse );
        t.expectFco(
                "(odd? 1)",
                bTrue );
        t.expectFco(
                "(odd? 2)",
                bFalse );

        t.expectFco(
                "(odd? -1.)",
                bTrue );
        t.expectFco(
                "(odd? 0.)",
                bFalse );
        t.expectFco(
                "(odd? 1.)",
                bTrue );
        t.expectFco(
                "(odd? 2.)",
                bFalse );

        t.expectError(
                "(odd? '())",
                Code.TYPE_ERROR );
        t.expectError(
                "(odd?)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
        t.expectError(
                "(odd? 0 0)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
    }

    /**
     * r7rs even? 6.2.6 p36
     */
    @Test
    public void evenq() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(even? -1)",
                bFalse );
        t.expectFco(
                "(even? 0)",
                bTrue );
        t.expectFco(
                "(even? 1)",
                bFalse );
        t.expectFco(
                "(even? 2)",
                bTrue );

        t.expectFco(
                "(even? -1.)",
                bFalse );
        t.expectFco(
                "(even? 0.)",
                bTrue );
        t.expectFco(
                "(even? 1.)",
                bFalse );
        t.expectFco(
                "(even? 2.)",
                bTrue );

        t.expectError(
                "(even? '())",
                Code.TYPE_ERROR );
        t.expectError(
                "(even?)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
        t.expectError(
                "(even? 0 0)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
    }

    /**
     * r7rs max 6.2.6 p36
     */
    @Test
    public void max() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(max 3 4)",
                i( 4 ) );
        t.expectFco(
                "(exact? (max 3 4))",
                bTrue );
        t.expectFco(
                "(max 3.9 4)",
                d( 4.0 ) );
        t.expectFco(
                "(inexact? (max 3.9 4))",
                bTrue );
    }

    /**
     * r7rs min 6.2.6 p36
     */
    @Test
    public void min() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(min 3 4)",
                i( 3 ) );
        t.expectFco(
                "(exact? (min 3 4))",
                bTrue );
        t.expectFco(
                "(min 3.9 4)",
                d( 3.9 ) );
        t.expectFco(
                "(inexact? (min 3.9 4))",
                bTrue );
    }

    /**
     * r7rs min 6.2.6 p36
     */
    @Test
    public void plus() throws Exception
    {
        var t = makeTester();

        {
            var fco = t.execute(
                    "(+ 300 10 3)" );
            assertInstanceOf(
                    SchemeInteger.class,
                    fco );
            assertEqualq(
                    i(313),
                    fco );
        }
        {
            var fco = t.execute(
                    "(+ 3 4)" );
            assertInstanceOf(
                    SchemeInteger.class,
                    fco );
            assertEqualq(
                    i(7),
                    fco );
        }
        {
            var fco = t.execute(
                    "(+ 3)" );
            assertInstanceOf(
                    SchemeInteger.class,
                    fco );
            assertEqualq(
                    i(3),
                    fco );
        }
        {
            var fco = t.execute(
                    "(+)" );
            assertInstanceOf(
                    SchemeInteger.class,
                    fco );
            assertEqualq(
                    i(0),
                    fco );
        }

        {
            var fco = t.execute(
                    "(+ 300 10. 3)" );
            assertInstanceOf(
                    SchemeDouble.class,
                    fco );
            assertEqualq(
                    d(313.),
                    fco );
        }
    }

    @Test
    public void multiply() throws Exception
    {
        var t = makeTester();

        {
            var fco = t.execute(
                    "(*)" );
            assertInstanceOf(
                    SchemeInteger.class,
                    fco );
            assertEqualq(
                    i(1),
                    fco );
        }
        {
            var fco = t.execute(
                    "(* 4)" );
            assertInstanceOf(
                    SchemeInteger.class,
                    fco );
            assertEqualq(
                    i(4),
                    fco );
        }
        {
            var fco = t.execute(
                    "(* 2 3)" );
            assertInstanceOf(
                    SchemeInteger.class,
                    fco );
            assertEqualq(
                    i(6),
                    fco );
        }

        {
            var fco = t.execute(
                    "(* 2. 3)" );
            assertInstanceOf(
                    SchemeDouble.class,
                    fco );
            assertEqualq(
                    d(6.),
                    fco );
        }
    }

    @Test
    public void minus() throws Exception
    {
        var t = makeTester();

        {
            var fco = t.execute(
                    "(- 3 4)" );
            assertInstanceOf(
                    SchemeInteger.class,
                    fco );
            assertEqualq(
                    i(-1),
                    fco );
        }
        {
            var fco = t.execute(
                    "(- 3 4 5)" );
            assertInstanceOf(
                    SchemeInteger.class,
                    fco );
            assertEqualq(
                    i(-6),
                    fco );
        }
        {
            var fco = t.execute(
                    "(- 3)" );
            assertInstanceOf(
                    SchemeInteger.class,
                    fco );
            assertEqualq(
                    i(-3),
                    fco );
        }

        {
            var fco = t.execute(
                    "(- 2. 3)" );
            assertInstanceOf(
                    SchemeDouble.class,
                    fco );
            assertEqualq(
                    d(-1.),
                    fco );
        }
    }

    @Test
    public void divide() throws Exception
    {
        var t = makeTester();

        {
            var fco = t.execute(
                    "(/ 4 2)" );
            assertInstanceOf(
                    SchemeInteger.class,
                    fco );
            assertEqualq(
                    i( 2 ),
                    fco );
        }
        {
            var fco = t.execute(
                    "(/ 1)" );
            assertInstanceOf(
                    SchemeInteger.class,
                    fco );
            assertEqualq(
                    i(1),
                    fco );
        }
        {
            var fco = t.execute(
                    "(/ 3)" );
            assertInstanceOf(
                    SchemeDouble.class,
                    fco );
            assertEqualq(
                    d(1./3.),
                    fco );
        }
        {
            var fco = t.execute(
                    "(/ 3 4 5)" );
            assertInstanceOf(
                    SchemeDouble.class,
                    fco );
            assertEqualq(
                    d(3./20.),
                    fco );
        }

        t.expectError(
                "(/ 1 '())",
                Code.TYPE_ERROR );
        t.expectError(
                "(/)",
                Code.NOT_ENOUGH_ARGUMENTS );
        t.expectError(
                "(/ 313 0)",
                Code.DIVISION_BY_ZERO );
        t.expectError(
                "(/ 313 .0)",
                Code.DIVISION_BY_ZERO );
    }

    @Test
    public void abs() throws Exception
    {
        var t = makeTester();

        {
            var fco = t.execute(
                    "(abs 4)" );
            assertInstanceOf(
                    SchemeInteger.class,
                    fco );
            assertEqualq(
                    i( 4 ),
                    fco );
        }
        {
            var fco = t.execute(
                    "(abs -4)" );
            assertInstanceOf(
                    SchemeInteger.class,
                    fco );
            assertEqualq(
                    i( 4 ),
                    fco );
        }
        {
            var fco = t.execute(
                    "(abs 2.0)" );
            assertInstanceOf(
                    SchemeDouble.class,
                    fco );
            assertEqualq(
                    d( 2. ),
                    fco );
        }
        {
            var fco = t.execute(
                    "(abs -2.0)" );
            assertInstanceOf(
                    SchemeDouble.class,
                    fco );
            assertEqualq(
                    d( 2. ),
                    fco );
        }

        t.expectError(
                "(abs '())",
                Code.TYPE_ERROR );
        t.expectError(
                "(abs)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
    }

    @Test
    public void floorDiv() throws Exception
    {
        var t = makeTester();

        // Define a support operation that performs the actual test.
        t.execute(
                """
                (define (check n1 n2 nqx nrx)
                  (let-values (((nq nr) (floor/ n1 n2)))
                    (and
                      (exact? nq)
                      (exact? nr)
                      (= nqx nq)
                      (= nrx nr))))
                """ );

        t.expectFco(
                "(check 5 2 2 1)", bTrue );
        t.expectFco(
                "(check -5 2 -3 1)", bTrue );
        t.expectFco(
                "(check 5 -2 -3 -1)", bTrue );
        t.expectFco(
                "(check -5 -2 2 -1)", bTrue );

        t.expectError(
                "(abs '())",
                Code.TYPE_ERROR );
        t.expectError(
                "(abs)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
    }

    @Test
    public void truncateDiv() throws Exception
    {
        var t = makeTester();

        // Define a support operation that performs the actual test.
        t.execute(
                """
                (define (check n1 n2 nqx nrx)
                  (let-values (((nq nr) (truncate/ n1 n2)))
                    (and
                      (exact? nq)
                      (exact? nr)
                      (= nqx nq)
                      (= nrx nr))))
                """ );

        t.expectFco(
                "(check 5 2 2 1)", bTrue );
        t.expectFco(
                "(check -5 2 -2 -1)", bTrue );
        t.expectFco(
                "(check 5 -2 -2 1)", bTrue );
        t.expectFco(
                "(check -5 -2 2 -1)", bTrue );

        t.expectError(
                "(abs '())",
                Code.TYPE_ERROR );
        t.expectError(
                "(abs)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
    }

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
    public void addition_err_1() throws Exception
    {
        expectError( "(+ 1 'i)", Code.TYPE_ERROR );
    }

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
