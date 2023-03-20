/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2023 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.SchemeBoolean;

/**
 * rsr7 6.10 Control features, p50
 *
 * @author MICBINZ
 */
public class R7rs_6_10_Control_features_Test extends ScreamBaseTest
{
    /**
     * p50
     */
    @Test
    public void procedureq_1() throws Exception
    {
        expectFco(
                "(procedure? car)",
                SchemeBoolean.T );
    }

    /**
     * p50
     */
    @Test
    public void procedureq_2() throws Exception
    {
        expectFco(
                "(procedure? 'car)",
                SchemeBoolean.F );
    }

    /**
     * p50
     */
    @Test
    public void procedureq_3() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (procedure? (lambda(x) (* x x)))
            """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p50
     */
    @Test
    public void procedureq_4() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (procedure? '(lambda(x) (* x x)))
            """ );
        assertEquals( SchemeBoolean.F, result );
    }

    /**
     * p50
     */
    @Test
    public void procedureq_5() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (call-with-current-continuation procedure?)
            """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p50
     */
    @Test
    public void apply_1() throws Exception
    {
        var se = scriptEngine();

        var result = se.evalFco(
            """
            (apply + (list 3 4))
            """ );
        assertEquals( i(7), result );
    }

    /**
     * p51
     */
    @Test
    public void apply_2() throws Exception
    {
        var se = scriptEngine();

        var result = se.evalFco(
            """
            (define compose
              (lambda (f g)
                (lambda args
                  (f (apply g args)))))
            ; The original example uses sqrt.
            ((compose - *) 12 75)
            """ );
        assertEquals( i(-900), result );
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
        assertEquals( SchemeBoolean.F, result );
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
        var se = scriptEngine();

        var result = se.evalFco(
"""
        (define return #f)

        (+ 1 (call-with-current-continuation
                (lambda (cont)
                    (set! return cont)
                    1)))

        (return 312)
"""  );

        assertEquals( i(313), result );
        assertEqualq( i(1), se.evalFco( "(return 0)" ) );
        assertEqualq( i(2), se.evalFco( "(return 1)" ) );
        assertEqualq( i(3), se.evalFco( "(return 2)" ) );
        assertEqualq( i(121), se.evalFco( "(return 120)" ) );
    }
}
