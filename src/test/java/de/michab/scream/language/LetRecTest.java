/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.SchemeBoolean;

/**
 * r7rs 4.2.2 Binding constructs, p16
 */
public class LetRecTest extends ScreamBaseTest
{
    /**
     * Checking the opposite of the r7rs test on p16.
     */
    @Test
    public void letrec_1x1() throws Exception
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
             (even? 89))
            """ );
        assertEquals( SchemeBoolean.F, result );
    }
    /**
     * Checking the opposite of the r7rs test on p16.
     */
    @Test
    public void letrec_1x2() throws Exception
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
             (odd? 89))
            """ );
        assertEquals( SchemeBoolean.T, result );
    }
    /**
     * Checking the opposite of the r7rs test on p16.
     */
    @Test
    public void letrec_1x3() throws Exception
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
             (odd? 88))
            """ );
        assertEquals( SchemeBoolean.F, result );
    }

}
