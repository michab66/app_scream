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
 * r7rs 4.2.2 Binding constructs, p16
 */
public class LetRecTest extends ScreamBaseTest
{
    /**
     * Checking the opposite of the r7rs test on p16.
     */
    @Test
    public void letrec_1x() throws Exception
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

}
