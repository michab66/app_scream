/*
 * Scream @ https://github.com/michab/dev_smack
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
        var result = scriptEngine().evalFco(
            """
            (procedure? car)
            """ );
        assertEquals( SchemeBoolean.T, result );
    }

    /**
     * p50
     */
    @Test
    public void procedureq_2() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (procedure? 'car)
            """ );
        assertEquals( SchemeBoolean.F, result );
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
    public void callWithCurrentContinuation_1() throws Exception
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
    public void callWithCurrentContinuation_2() throws Exception
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
    public void callWithCurrentContinuation_3() throws Exception
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
}
