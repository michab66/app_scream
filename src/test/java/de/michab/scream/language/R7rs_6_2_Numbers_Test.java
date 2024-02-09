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
import de.michab.scream.fcos.Bool;
import de.michab.scream.fcos.Int;

public class R7rs_6_2_Numbers_Test extends ScreamBaseTest
{
    @Test
    public void unimplemented() throws Exception
    {
        var t = makeTester();

        t.expectError(
                "(make-rectangular 1 2)",
                Code.NOT_IMPLEMENTED );
        t.expectError(
                "(make-polar 1 2)",
                Code.NOT_IMPLEMENTED );
        t.expectError(
                "(real-part 1)",
                Code.NOT_IMPLEMENTED );
        t.expectError(
                "(imag-part 1)",
                Code.NOT_IMPLEMENTED );
        t.expectError(
                "(magnitude 1)",
                Code.NOT_IMPLEMENTED );
        t.expectError(
                "(angle 1)",
                Code.NOT_IMPLEMENTED );
        t.expectError(
                "(finite? 1)",
                Code.NOT_IMPLEMENTED );
        t.expectError(
                "(infinite? 1)",
                Code.NOT_IMPLEMENTED );
        t.expectError(
                "(nan? 1)",
                Code.NOT_IMPLEMENTED );
    }

    /**
     * r7rs exact/inexact 6.2.6 p35
     */
    @Test
    public void predicates() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(number? 3)",
                Bool.T );
        t.expectFco(
                "(number? 3.13)",
                Bool.T );
        t.expectFco(
                "(number? 'x)",
                Bool.F );
        t.expectError(
                "(complex? 1)",
                Code.NOT_IMPLEMENTED );
        t.expectFco(
                "(real? 3)",
                Bool.T );
        t.expectFco(
                "(real? 3.)",
                Bool.T );
        t.expectError(
                "(rational? 3.5)",
                Code.NOT_IMPLEMENTED );
        t.expectFco(
                "(integer? 3.)",
                Bool.T );
        t.expectFco(
                "(integer? 3)",
                Bool.T );
        t.expectFco(
                "(integer? 3.13)",
                Bool.F );
    }

    /**
     * r7rs exact/inexact 6.2.6 p35
     */
    @Test
    public void exact_inexact_integerq() throws Exception
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
                bFalse );
        t.expectError(
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
                "(max 3.9 4)",
                d( 4.0 ) );
        t.expectError(
                "(max 3.9 4 'x)",
                Code.TYPE_ERROR );
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
                "(min 3.9 4)",
                d( 3.9 ) );
        t.expectError(
                "(min 3.9 4 'x)",
                Code.TYPE_ERROR );
    }

    /**
     * r7rs min 6.2.6 p36
     */
    @Test
    public void plus() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(+ 300 10 3)",
                i(313) );
        t.expectFco(
                "(+ 3 4)",
                i(7) );
        t.expectFco(
                "(+ 3)",
                i(3) );
        t.expectFco(
                "(+)",
                i(0) );
        t.expectFco(
                "(+ 300 10. 3)",
                d(313.) );
        t.expectError(
                "(+ 1 'i)",
                Code.TYPE_ERROR );
    }

    @Test
    public void multiply() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(*)",
                i(1) );
        t.expectFco(
                "(* 4)",
                i(4) );
        t.expectFco(
                "(* 2 3)",
                i(6) );
        t.expectFco(
                "(* 2. 3)",
                d(6.) );
    }

    @Test
    public void minus() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(- 3 4)",
                i(-1) );
        t.expectFco(
                "(- 3 4 5)",
                i(-6) );
        t.expectFco(
                "(- 3)",
                i(-3) );
        t.expectFco(
                "(- 2. 3)",
                d(-1.) );
    }

    @Test
    public void divide() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(/ 4 2)",
                i( 2 ) );
        t.expectFco(
                "(/ 4.0 2)",
                d( 2. ) );
        t.expectFco(
                "(/ 4 2.0)",
                d( 2. ) );
        t.expectFco(
                "(/ 1)",
                i(1) );
        t.expectFco(
                "(/ 1.0)",
                d(1) );
        t.expectFco(
                "(/ 3)",
                d(1./3.) );
        t.expectFco(
                "(/ 3 4 5)",
                d(3./20.) );
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

        t.expectFco(
                "(abs 4)",
                i( 4 ) );
        t.expectFco(
                "(abs -4)",
                i( 4 ) );
        t.expectFco(
                "(abs 2.0)",
                d( 2. ) );
        t.expectFco(
                "(abs -2.0)",
                d( 2. ) );

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
                (define (check n1 n2)
                  (let-values (((nq nr) (floor/ n1 n2)))
                    (cons nq nr)))
                """ );

        t.expectFco(
                "(check 5 2)",
                "(2 . 1)" );
        t.expectFco(
                "(check -5 2)",
                "(-3 . 1)" );
        t.expectFco(
                "(check 5 -2)",
                "(-3 . -1)" );
        t.expectFco(
                "(check -5 -2)",
                "(2 . -1)" );
        t.expectFco(
                "(check -5. -2)",
                "(2. . -1.)" );
        t.expectFco(
                "(check -5 -2.)",
                "(2. . -1.)" );

        t.expectError(
                "(floor/ '() '())",
                Code.TYPE_ERROR );
        t.expectError(
                "(floor/)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
        t.expectError(
                "(floor/ 1 2 3)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
    }

    @Test
    public void truncateDiv() throws Exception
    {
        var t = makeTester();

        // Define a support operation that performs the actual test.
        t.execute(
                """
                (define (check n1 n2)
                  (let-values (((nq nr) (truncate/ n1 n2)))
                    (cons nq nr)))
                """ );

        t.expectFco(
                "(check 5 2)",
                "(2 . 1)" );
        t.expectFco(
                "(check -5 2)",
                "(-2 . -1)" );
        t.expectFco(
                "(check 5 -2)",
                "(-2 . 1)" );
        t.expectFco(
                "(check -5 -2)",
                "(2 . -1)" );
        t.expectFco(
                "(check -5.0 -2)",
                "(2. . -1.)" );

        t.expectError(
                "(truncate/ '() '())",
                Code.TYPE_ERROR );
        t.expectError(
                "(truncate/)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
        t.expectError(
                "(truncate/ 1 2 3)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
    }

    @Test
    public void quotient_remainder_modulo() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(eqv? quotient truncate-quotient)",
                bTrue );
        t.expectFco(
                "(eqv? remainder truncate-remainder)",
                bTrue );
        t.expectFco(
                "(eqv? modulo floor-remainder)",
                bTrue );
    }

    @Test
    public void gcd() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(gcd 32 -36)",
                i(4) );
        t.expectFco(
                "(gcd 32.0 -36)",
                d(4.) );
        t.expectFco(
                "(gcd)",
                i(0) );
    }

    @Test
    public void lcm() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(lcm 32 -36)",
                i(288) );
        t.expectFco(
                "(lcm 32.0 -36)",
                d(288.) );
        t.expectFco(
                "(lcm)",
                i(1) );
    }

    @Test
    public void floor() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(floor -4.3)",
                d(-5.) );
        t.expectFco(
                "(floor 3.5)",
                d(3.) );
        t.expectFco(
                "(floor 313)",
                i(313) );
        t.expectError(
                "(floor '())",
                Code.TYPE_ERROR );
        t.expectError(
                "(floor)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
        t.expectError(
                "(floor 1 2)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
    }

    @Test
    public void ceiling() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(ceiling -4.3)",
                d(-4.) );
        t.expectFco(
                "(ceiling 3.5)",
                d(4.) );
        t.expectFco(
                "(ceiling 313)",
                i(313) );
        t.expectError(
                "(ceiling '())",
                Code.TYPE_ERROR );
        t.expectError(
                "(ceiling)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
        t.expectError(
                "(ceiling 1 2)",
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
        t.expectError(
                "(truncate '())",
                Code.TYPE_ERROR );
        t.expectError(
                "(truncate)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
        t.expectError(
                "(truncate 1 2)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
    }

    @Test
    public void round() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(round -4.3)",
                d( -4. ) );
        t.expectFco(
                "(round 3.5)",
                d( 4. ) );
        t.expectFco(
                "(round 7)",
                i( 7 ) );
        t.expectError(
                "(round '())",
                Code.TYPE_ERROR );
        t.expectError(
                "(round)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
        t.expectError(
                "(round 1 2)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
    }

    @Test
    public void transcendental() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(exp 1)",
                d( Math.exp( 1 ) ) );
        t.expectError(
                "(exp '())",
                Code.TYPE_ERROR);

        t.expectFco(
                "(log 2)",
                d( Math.log( 2 ) ) );
        t.expectError(
                "(log '())",
                Code.TYPE_ERROR );

        t.expectFco(
                "(log 2 3)",
                d( Math.log( 2 ) / Math.log( 3 ) ) );
        t.expectError(
                "(log '() 3)",
                Code.TYPE_ERROR );
        t.expectError(
                "(log 2 '())",
                Code.TYPE_ERROR );

        t.expectFco(
                "(sin 2)",
                d( Math.sin( 2. ) ) );
        t.expectError(
                "(sin '())",
                Code.TYPE_ERROR );

        t.expectFco(
                "(cos 2)",
                d( Math.cos( 2. ) ) );
        t.expectError(
                "(cos '())",
                Code.TYPE_ERROR );

        t.expectFco(
                "(tan 2)",
                d( Math.tan( 2. ) ) );
        t.expectError(
                "(tan '())",
                Code.TYPE_ERROR );

        t.expectFco(
                "(asin 1)",
                d( Math.asin( 1. ) ) );
        t.expectError(
                "(asin '())",
                Code.TYPE_ERROR );

        t.expectFco(
                "(acos 1)",
                d( Math.acos( 1. ) ) );
        t.expectError(
                "(acos '())",
                Code.TYPE_ERROR );

        t.expectFco(
                "(atan 1)",
                d( Math.atan( 1. ) ) );
        t.expectError(
                "(atan '())",
                Code.TYPE_ERROR );

        t.expectFco(
                "(atan 1 2)",
                d( Math.atan2( 1., 2. ) ) );
        t.expectError(
                "(atan '() 2)",
                Code.TYPE_ERROR );
        t.expectError(
                "(atan 1 '())",
                Code.TYPE_ERROR );

    }

    @Test
    public void square() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(square 42)",
                i( 1764 ) );
        t.expectFco(
                "(square 2.0)",
                d( 4. ) );
        t.expectError(
                "(square '())",
                Code.TYPE_ERROR );
    }

    @Test
    public void sqrt() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(sqrt 9)",
                i( 3 ) );
        t.expectFco(
                "(sqrt 9.0)",
                d( 3 ) );
        t.expectError(
                "(sqrt '())",
                Code.TYPE_ERROR );
    }

    @Test
    public void expt() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(expt 2 3)",
                i( 8 ) );
        t.expectFco(
                "(expt 2. 3)",
                d( 8. ) );
        t.expectFco(
                "(expt 2 3.)",
                d( 8. ) );
        t.expectError(
                "(expt '() 9)",
                Code.TYPE_ERROR );
    }

    /**
     * r7rs exact/inexact 6.2.6 p35
     */
    @Test
    public void exactq_inexactq() throws Exception
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

    @Test
    public void inexact() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(inexact 3.1415)",
                d( 3.1415 ) );

        t.expectFco(
                "(inexact 3)",
                d( 3.0 ) );
        t.expectFco(
                "(inexact 3.0)",
                d( 3 ) );
    }

    @Test
    public void exact() throws Exception
    {
        var t = makeTester();

        assertInstanceOf(
                Int.class,
                t.execute( "(exact 313)" ) );

        t.expectFco(
                "(exact 313)",
                i( 313 ) );
        t.expectFco(
                "(exact 3.0)",
                i( 3 ) );
        t.expectError(
                "(exact 3.13)",
                Code.ILLEGAL_ARGUMENT );
    }
}
