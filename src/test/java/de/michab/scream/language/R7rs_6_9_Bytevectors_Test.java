/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2023 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamException.Code;

/**
 * rsr7 6.9 Bytevectors, p50
 *
 * @author MICBINZ
 */
public class R7rs_6_9_Bytevectors_Test extends ScreamBaseTest
{
    /**
     * p49
     */
    @Test
    public void basic() throws Exception
    {
        expectFco(
                "#u8(0 10 5)",
                parse( "#u8(0 10 5)" ) );
    }

    /**
     * p49
     */
    @Test
    public void basic_type_error() throws Exception
    {
        var e = expectError(
                "#u8(0 x 5)",
                Code.PARSE_EXPECTED );
    }

    /**
     * p49
     */
    @Test
    public void basic_range_error() throws Exception
    {
        var e = expectError(
                "#u8(0 313 5)",
                Code.RANGE_EXCEEDED );
    }

    /**
     * p49
     */
    @Test
    public void bytevector_q() throws Exception
    {
        expectFco(
                "(bytevector? #u8(3 1 3))",
                bTrue );
        expectFco(
                "(bytevector? #(3 1 3))",
                bFalse );
        expectFco(
                "(bytevector? 313)",
                bFalse );
        expectFco(
                "(bytevector? '(1 2 3))",
                bFalse );
        expectFco(
                "(bytevector? '())",
                bFalse );
        expectFco(
                "(bytevector? 'donald)",
                bFalse );
    }

    /**
     * p49
     */
    @Test
    public void bytevector() throws Exception
    {
        expectFco(
                "(bytevector? (bytevector 3 1 3))",
                bTrue );

        {
        var e = expectError(
                "(bytevector 3 1 256)",
                Code.RANGE_EXCEEDED );
        assertEquals( i(256), e.getArgument( 0 ) );
        }
        {
        var e = expectError(
                "(bytevector 3 1 -1)",
                Code.RANGE_EXCEEDED );
        assertEquals( i(-1), e.getArgument( 0 ) );
        }
    }

    /**
     * p50
     */
    @Test
    public void bytevector_length() throws Exception
    {
        expectFco(
                "(bytevector-length (bytevector 255 1 3))",
                i3 );

        expectError(
                "(bytevector-length 3)",
                Code.TYPE_ERROR );
    }

    /**
     * p50
     */
    @Test
    public void bytevector_set() throws Exception
    {
        expectFco(
"""
        (let ((bv (bytevector 1 2 3 4)))
          (bytevector-set! bv 1 3)
          bv)

""",
        parse( "#u8(1 3 3 4)" ) );
    }
}
