/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.language;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.SchemeBoolean;

/**
 * r7rs 4.2.2 Binding constructs, p16
 *
 * @author micbinz
 */
public class R7rs_4_2_2_Binding_constructs_Test extends ScreamBaseTest
{
    /**
     * p16
     */
    @Test
    public void let_1() throws Exception
    {
        expectFco(
            """
            (let ((x 2) (y 3))
              (+ x y))
            """,
            i(5) );
    }

    /**
     * p16
     */
    @Test
    public void let_2() throws Exception
    {
        expectFco(
            """
            (let ((x 2) (y 3))
              (let ((x 7)
                    (z (+ x y)))
                    (* z x)))
            """,
            i(35) );
    }

    /**
     * p16
     */
    @Test
    public void letAsterisk_1() throws Exception
    {
        expectFco(
            """
            (let ((x 2) (y 3))
              (let* ((x 7)
                    (z (+ x y)))
                    (* z x)))
            """,
            i(70) );
    }

    /**
     * p16
     */
    @Test
    public void letrec_1() throws Exception
    {
        expectFco(
            """
            (letrec ((even?
                      (lambda (n)
                        (if (zero? n)
                            #t
                            (odd? (- n 1)))))
                     (odd?
                       (lambda (n)
                         (if (zero? n)
                            #f
                            (even? (- n 1))))))
             (even? 88))
            """,
            SchemeBoolean.T );
    }

    /**
     * p17
     */
    @Test
    public void let_values_1() throws Exception
    {
        expectFco(
"""
        (let-values (((root rem) (exact-integer-sqrt 32)))
          (* root rem))
""",
        i(35) );
    }

    /**
     * p17
     */
    @Test
    public void let_star_values_1() throws Exception
    {
        expectFco(
"""
        (let ((a 'a) (b 'b) (x 'x) (y 'y))
          (let*-values (((a b) (values x y))
                               ((x y) (values a b)))
            (list a b x y)))
""",
        "(x y x y)" );
    }
}
