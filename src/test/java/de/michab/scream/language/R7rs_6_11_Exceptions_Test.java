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
                "(call-with-current-continuation\n"
                + "  (lambda (k)\n"
                + "    (with-exception-handler\n"
                + "      (lambda (x)\n"
                + "        (display \"condition: \")\n"
                + "        (write x)\n"
                + "        (newline)\n"
                + "        (k 'exception))\n"
                + "      (lambda () \n"
                + "        (+ 1 (raise 'an-error))))))",
                "exception" );

            assertEquals( "condition: an-error", x.content().get( 0 ) );
        }
    }
}
