/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2023 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.smack.util.io.Redirect;
import org.smack.util.io.Redirect.StdStream;

import de.michab.scream.ScreamBaseTest;

/**
 * rsr7 6.11 Exceptions, p54
 *
 * @author micbinz
 */
public class R7rs_6_11_Exceptions_Test extends ScreamBaseTest
{
    /**
     * p54
     */
    @Test
    public void withExceptionHandler() throws Exception
    {
        try ( var x = new Redirect( StdStream.out ) )
        {
            expectFco(
"""
            (call-with-current-continuation
              (lambda (k)
                (with-exception-handler
                  (lambda (x)
                    (display "condition: ")
                    (write x)
                    (newline)
                    (k 'exception))
              (lambda ()
                (+ 1 (raise 'an-error))))))
""",
                "exception" );

            assertEquals( "condition: an-error", x.content().get( 0 ) );
        }
    }

    /**
     * https://www.scheme.com/tspl4/exceptions.html
     */
    @Test
    public void schemeCom_11_1_RaisingAndHandlingExceptions() throws Exception
    {
            expectFco(
"""
            (list
              (call/cc
                (lambda (k)
                  (vector
                    (with-exception-handler
                      (lambda (x) (k (+ x 5)))
                      (lambda () (+ (raise 17) 8)))))))
""",
                "(22)" );

    }
}
