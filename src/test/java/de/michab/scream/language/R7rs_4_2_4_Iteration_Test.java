/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.language;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;

/**
 * r7rs 4.2.4 Iteration.
 *
 * @author MICBINZ
 */
public class R7rs_4_2_4_Iteration_Test extends ScreamBaseTest
{
    /**
     * p18
     */
    @Test
    public void do_1() throws Exception
    {
        expectFco(
            """
            (do ((vec (make-vector 5))
                 (i 0 (+ i 1)))
                ((= i 5) vec)
              (vector-set! vec i i))
            """,
            "#(0 1 2 3 4)" );
    }

    /**
     * p18
     */
    @Test
    public void do_1x() throws Exception
    {
        expectFco(
"""
        (do ((vec (make-vector 7))
             (i 0 (+ i 1)))
            ((= i (vector-length vec)) vec)
          (vector-set! vec i i))
""",
        "#(0 1 2 3 4 5 6)" );
    }

    /**
     * p18
     */
    @Test
    public void do_2() throws Exception
    {
        expectFco(
"""
        (let ((x '(1 3 5 7 9)))
          (do ((x x (cdr x))
               (sum 0 (+ sum (car x))))
              ((null? x) sum)))
""",
        i(25) );
    }

    /**
     * Issue #186 : https://github.com/urschleim/scream/issues/186
     */
    @Test
    public void do_186() throws Exception
    {
        expectFco(
"""
        (define (range-test b e)
          (do ((r '() (cons e r))
                (e (- e 1) (- e 1)))
               ((< e b) r)))
        (range-test 3 5)
""",
        "(3 4)" );
    }

    /**
     * r7rs p18
     */
    @Test
    public void named_let_1() throws Exception
    {
        expectFco(
"""
        (let loop
            ((numbers '(3 -2 1 6 -5))
            (nonneg '())
            (neg '()))
          (cond
            ((null? numbers) (list nonneg neg))
            ((>= (car numbers) 0)
                  (loop (cdr numbers)
                        (cons (car numbers) nonneg)
                        neg))
            (else
              (loop (cdr numbers)
                    nonneg
                    (cons (car numbers) neg)))))
""",
        "((6 1 3) (-5 -2))" );
    }

    /**
     * SO: https://stackoverflow.com/questions/31909121/how-does-the-named-let-in-the-form-of-a-loop-work
     */
    @Test
    public void named_let_basic() throws Exception
    {
        expectFco(
"""
        (let ((loop 10))
          (let loop ((n loop))
            (if (zero? n)
                '()
                (cons n (loop (- n 1))))))
""",
        "(10 9 8 7 6 5 4 3 2 1)" );
    }

}
