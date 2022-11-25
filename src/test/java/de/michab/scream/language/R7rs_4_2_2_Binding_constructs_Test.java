/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import de.michab.scream.SchemeBoolean;
import de.michab.scream.ScreamBaseTest;

/**
 * r7rs 4.2.2 Binding constructs.
 *
 * @author MICBINZ
 */
public class R7rs_4_2_2_Binding_constructs_Test extends ScreamBaseTest
{
    /**
     * p16
     */
    @Test
    public void let_1() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (let ((x 2) (y 3))
              (+ x y))
            """ );
        assertEquals( i(5), result );
    }

    /**
     * p16
     */
    @Test
    public void let_2() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (let ((x 2) (y 3))
              (let ((x 7)
                    (z (+ x y)))
                    (* z x)))
            """ );
        assertEquals( i(35), result );
    }

    /**
     * p16
     */
    @Test
    public void letAsterisk_1() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (let ((x 2) (y 3))
              (let* ((x 7)
                    (z (+ x y)))
                    (* z x)))
            """ );
        assertEquals( i(70), result );
    }

    /**
     * p16
     */
    @Test
    public void letrec_1() throws Exception
    {
        var result = scriptEngine().evalFco(
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
            """ );
        assertEquals( SchemeBoolean.T, result );
    }

}
