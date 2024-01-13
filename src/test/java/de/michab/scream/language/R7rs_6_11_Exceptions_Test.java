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

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;

/**
 * rsr7 6.11 Exceptions, p54
 *
 * @author micbinz
 */
public class R7rs_6_11_Exceptions_Test extends ScreamBaseTest
{
    /**
     * r7rs 6.11 p54
     */
    @Test
    public void withExceptionHandler() throws Exception
    {
        try ( var stdout = new Redirect( StdStream.out ) )
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

            assertEquals(
                    "condition: an-error",
                    stdout.content().get( 0 ) );
        }
    }

    /**
     * r7rs 6.11 p54
     */
    @Test
    public void withExceptionHandler2() throws Exception
    {
        try ( var stdout = new Redirect( StdStream.out ) )
        {
            expectError(
"""
            (with-exception-handler
              (lambda (x)
                (display "something went wrong\\n"))
              (lambda ()
                (+ 1 (raise 'an-error))))
""",
              Code.NOT_CONTINUABLE );

            assertEquals(
                    "something went wrong",
                    stdout.content().get( 0 ) );
        }
    }

    /**
     * r7rs 6.11 p54
     */
    @Test
    public void raiseContinuable() throws Exception
    {
        try ( var stdout = new Redirect( StdStream.out ) )
        {
            expectFco(
"""
            (with-exception-handler
              (lambda (con)
                (cond
                  ((string? con)
                   (display con))
                  (else
                   (display "a warning has been issued")))
                42)
              (lambda ()
                (+ (raise-continuable "should be a number")
                   23)))
""",
              "65" );

            assertEquals(
                    "should be a number",
                    stdout.content().get( 0 ) );
        }
    }

    /**
     * r7rs 6.11 p54
     */
    @Test
    public void callWithCc_errorInHandler() throws Exception
    {
            expectError(
"""
            (with-exception-handler
              (lambda (con)
                not-defined)
              (lambda ()
                (raise 'not-used)))
""",
              Code.SYMBOL_NOT_DEFINED );
    }
    /**
     * r7rs 6.11 p54
     */
    @Test
    public void callWithCc_errorInThunk() throws Exception
    {
            expectError(
"""
            (with-exception-handler
              (lambda (con)
                1)
              (lambda ()
                not-defined
                (raise 'not-used)))
""",
              Code.SYMBOL_NOT_DEFINED );
    }

    /**
     * https://www.scheme.com/tspl4/exceptions.html
     */
    @Test
    public void schemeCom_11_1_RaisingAndHandlingExceptions_1() throws Exception
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

    /**
     * https://www.scheme.com/tspl4/exceptions.html
     */
    @Test
    public void schemeCom_11_1_RaisingAndHandlingExceptions_2() throws Exception
    {
            expectFco(
"""
            (list
              (vector
                (with-exception-handler
                  (lambda (x) (+ x 5))
                  (lambda () (+ (raise-continuable 17) 8)))))
""",
                "(#(30))" );
    }

    /**
     * https://www.scheme.com/tspl4/exceptions.html
     */
    @Test
    public void schemeCom_11_1_RaisingAndHandlingExceptions_3() throws Exception
    {
            expectError(
"""
            (list
              (vector
                (with-exception-handler
                  (lambda (x) (+ x 5))
                  (lambda () (+ (raise 17) 8)))))
""",
                Code.NOT_CONTINUABLE );
    }
}
