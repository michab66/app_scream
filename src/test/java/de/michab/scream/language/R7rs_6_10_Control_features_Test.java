/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2023 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Bool;

/**
 * r7rs 6.10 Control features, p50
 *
 * @author MICBINZ
 */
public class R7rs_6_10_Control_features_Test extends ScreamBaseTest
{
    /**
     * p50
     */
    @Test
    public void procedureq() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(procedure? car)",
                bTrue );
        t.expectFco(
                "(procedure? 'car)",
                bFalse );
        t.expectFco(
                """
                (procedure? (lambda(x) (* x x)))
                """,
                bTrue );
        t.expectFco(
                """
                (procedure? '(lambda(x) (* x x)))
                """,
                bFalse );
        t.expectFco(
                """
                (call-with-current-continuation procedure?)
                """,
                bTrue );
    }

    /**
     * p50
     */
    @Test
    public void apply() throws Exception
    {
        var t = makeTester();

        t.expectFco(
            """
            (apply + (list 3 4))
            """,
            i(7) );

        t.expectFco(
            """
            (define compose
              (lambda (f g)
                (lambda args
                  (f (apply g args)))))
            ((compose sqrt *) 12 75)
            """,
            i(30) );

        t.expectFco(
                "(apply + 3 '(4))",
                "7" );

        t.expectFco(
                "(apply + 2 3 '(4))",
                " 9 " );

        t.expectFco(
                "(apply + 10 '(2 1))",
                i(13) );
        t.expectFco(
                "(apply + 300 10 '(2 1))",
                i313 );
        t.expectFco(
                "(apply cadr '((a b)))",
                s("b") );

        // Last argument wrong.
        t.expectError(
                "(apply + 300 10 3)",
                Code.TYPE_ERROR );
        // Not a procedure.
        t.expectError(
                "(apply 1 300 10 '(2 1))",
                Code.TYPE_ERROR );
    }

    /**
     * p51
     */
    @Test
    public void map() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(map cadr '((a b)(d e)(g h)))",
                "(b e h)" );
        t.expectFco(
                "(map (lambda (n) (expt n n))  '(1 2 3 4 5))",
                "(1 4 27 256 3125)" );
        t.expectFco(
                "(map + '(1 2 3) '(4 5 6 7))",
                "(5 7 9)" );
        t.expectError(
                "(map +)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
        t.expectError(
                "(map + (scream:make-circular! (list 1 2)) (scream:make-circular! (list 3 4)))",
                Code.ILLEGAL_ARGUMENT );
    }

    /**
     * p51
     */
    @Test
    public void string_map() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(string-map char-upcase \"donald\")",
                str( "DONALD" ) );
    }

    /**
     * p51
     */
    @Test
    @Disabled( "https://github.com/urschleim/scream/issues/281" )
    public void string_map_2() throws Exception
    {
        expectFco(
                """
                (string-map
                  (lambda (c)
                    (integer->char (+ 1 (char->integer c))))
                  \"HAL\" )
                """,
                str( "IBM" ) );
    }

    @Test
    public void string_map_3() throws Exception
    {
        expectFco(
                """
                (string-map
                    (lambda (c k)
                        ((if (eqv? k #\\u) char-upcase char-downcase)
                        c))
                    "studlycaps xxx"
                    "ululululul")
                """,
                str( "StUdLyCaPs" ) );
    }

    @Test
    public void vector_map_1() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                """
                (vector-map cadr '#((a b)(d e)(g h)))
                """,
                "#(b e h)" );
        t.expectFco(
                """
                (vector-map (lambda (n) (expt n n))
                '#(1 2 3 4 5))
                """,
                "#(1 4 27 256 3125)" );
        t.expectFco(
                """
                (vector-map + '#(1 2 3) '#(4 5 6 7))
                """,
                "#(5 7 9)" );
        t.expectFco(
                """
                (let ((count 0))
                  (vector-map
                    (lambda (ignored)
                      (set! count (+ count 1))
                      count)
                    '#(a b)))
                """,
                // scream processes the map-calls left to right.
                "#(1 2)" );
    }

    /**
     * p51
     */
    @Test
    public void for_each_1() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                """
                (let ((v (make-vector 5)))
                  (for-each
                    (lambda (i)
                      (vector-set! v i (* i i)))
                    '(0 1 2 3 4))
                  v)
                """,
                "#(0 1 4 9 16)" );
        t.expectFco(
                """
                (let ((v '()))
                  (string-for-each
                    (lambda (c)
                      (set! v (cons (char->integer c) v)))
                    "abcde")
                  v)
                """,
                "(101 100 99 98 97)" );
        t.expectFco(
                """
                (let ((v (make-list 5)))
                  (vector-for-each
                    (lambda (i)
                      (list-set! v i (* i i)))
                      #(0 1 2 3 4))
                  v)
                """,
                "(0 1 4 9 16)" );
    }

    /**
     * p52
     */
    @Test
    public void call_cc__r7rs_1() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (call-with-current-continuation
              (lambda (exit)
                (for-each (lambda (x)
                            (if (negative? x)
                                (exit x)))
                          '(54 0 37 -3 245 19))
                #t))
            """ );
        assertEquals( i(-3), result );
    }

    @Test
    public void call_cc__r7rs_2() throws Exception
    {
        var se = scriptEngine();

        var result = se.evalFco(
            """
            (define list-length
              (lambda (obj)
                (call-with-current-continuation
                  (lambda (return)
                    (letrec ((r
                              (lambda (obj)
                                (cond ((null? obj) 0)
                                      ((pair? obj) (+ (r (cdr obj)) 1))
                                      (else (return #f))))))
                      (r obj))))))

            (list-length '(1 2 3 4))
            """ );
        assertEquals( i(4), result );
    }

    @Test
    public void call_cc__r7rs_3() throws Exception
    {
        var se = scriptEngine();

        var result = se.evalFco(
            """
            (define list-length
              (lambda (obj)
                (call/cc
                  (lambda (return)
                    (letrec ((r
                              (lambda (obj)
                                (cond ((null? obj) 0)
                                      ((pair? obj) (+ (r (cdr obj)) 1))
                                      (else (return #f))))))
                      (r obj))))))


            (list-length '(a b . c))
            """ );
        assertEquals( Bool.F, result );
    }

    /**
     * Leave a pending computation.
     */
    @Test
    public void call_cc__cancel() throws Exception
    {
        expectFco(
                "(call-with-current-continuation (lambda (k) (* 5 (k 4))))",
                i4 );
    }

    /**
     * Restart computation.
     */
    @Test
    public void call_cc_restart() throws Exception
    {
        var se = scriptEngine();

        var result = se.evalFco(
"""
        (define return #f)

        (+ 1 (call-with-current-continuation
                (lambda (cont)
                    ; Catch the continuation in the free,
                    ; top-level variable.
                    (set! return cont)
                    1)))

        ; Restart the continuation several times.
        (return 310)
        (return 311)
        (return 312)
""" );
        assertEqualq( i(313), result );
    }

    /**
     * Restart computation.
     */
    @Test
    public void call_cc_restart_2() throws Exception
    {
        var se = expectFco(
"""
        (define return #f)

        (+ 1 (call-with-current-continuation
                (lambda (cont)
                    (set! return cont)
                    1)))
""",
        i2 );

        assertEqualq( i(1), se.evalFco( "(return 0)" ) );
        assertEqualq( i(2), se.evalFco( "(return 1)" ) );
        assertEqualq( i(3), se.evalFco( "(return 2)" ) );
        assertEqualq( i(121), se.evalFco( "(return 120)" ) );
    }

    @Test
    public void values() throws Exception
    {
        expectFco(
"""
        (values 1 2)
""",
        "(1 2)" );
    }

    @Test
    public void call_with_values() throws Exception
    {
        var t = makeTester();

        t.expectFco(
"""
        (call-with-values
            (lambda () (values 4 5))
            (lambda (a b) b))
""",
        i(5) );

        t.expectFco(
"""
        (call-with-values
            (lambda () (values 300 10 3))
            (lambda (a b c) (+ a b c)))
""",
         i(313) );

        t.expectError(
"""
        (call-with-values
            (lambda () (values 300 10 3))
            (lambda (a b) (+ a b)))
""",
        Code.WRONG_NUMBER_OF_ARGUMENTS);
    }
}
