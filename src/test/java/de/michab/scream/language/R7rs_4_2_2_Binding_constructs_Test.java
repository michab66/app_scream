/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX.Code;
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
    @Test
    public void let_error_binding_visibility() throws Exception
    {
        var rx = expectError(
            """
            (let
              (
                (x 2)
                (y x)
              )
              y)
            """,
            Code.SYMBOL_NOT_DEFINED );

        assertEquals(
                "x",
                rx.getArgument( 0 ).toString() );
    }

    /**
     * p16
     */
    @Test
    public void let_error_duplicate() throws Exception
    {
        var rx = expectError(
            """
            (let
              (
                (x 2)
                (a 2)
                (b 2)
                (c 2)
                (d 2)
                (e 2)
                (f 2)
                (g 2)
                (x 3)
              )
              x)
            """,
            Code.DUPLICATE_ELEMENT );

        assertEquals(
                "x",
                rx.getArgument( 0 ).toString() );
    }

    /**
     * p16
     */
    @Test
    public void letStar_0() throws Exception
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

    @Test
    public void letStar_1() throws Exception
    {
        // x is defined in the second initialization
        // in difference to plain let.
        expectFco(
            """
            (let*
              (
                (x 2)
                (y x)
              )
              y)
            """,
            i2 );
    }

    @Test
    public void letStar_2() throws Exception
    {
        // r7rs: The <variable>s need not be distinct.
        expectFco(
            """
            (let*
              (
                (x 2)
                (x (+ x 310))
                (x (+ 1 x))
                (y x)
              )
              y)
            """,
            i313 );
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
