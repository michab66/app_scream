/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2023 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;

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
        assertEquals( "256", e.getArgument( 0 ) );
        }

        expectFco(
                "(bytevector 3 1 -1)",
                parse( "#u8(3 1 255)" ) );
    }

    public void make_bytevector() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "make-bytevector 0 13",
                parse( "#u8(#x0d #x0d)" ) );
        t.expectFco(
                "make-bytevector 2 13",
                parse( "#u8()" ) );
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
    public void bytevector_setq() throws Exception
    {
        expectFco(
"""
        (let ((bv (bytevector 1 2 3 4)))
          (bytevector-set! bv 1 3)
          bv)

""",
        parse( "#u8(1 3 3 4)" ) );
    }

    /**
     * p50
     */
    @Test
    public void bytevector_copy_1() throws Exception
    {
        expectFco(
"""
        (define a #u8(1 2 3 4 5))
        (bytevector-copy a)
""",
        parse( "#u8(1 2 3 4 5)" ) );
    }

    /**
     * p50
     */
    @Test
    public void bytevector_copy_2() throws Exception
    {
        expectFco(
"""
        (define a #u8(1 2 3 4 5))
        (bytevector-copy a 2)
""",
        parse( "#u8(3 4 5)" ) );
    }

    /**
     * p50
     */
    @Test
    public void bytevector_copy_2_empty() throws Exception
    {
        expectFco(
"""
        (define a #u8(1 2 3 4 5))
        (bytevector-copy a 5)
""",
        parse( "#u8()" ) );
    }

    /**
     * p50
     */
    @Test
    public void bytevector_copy_2_err_m1() throws Exception
    {
        expectError(
"""
        (define a #u8(1 2 3 4 5))
        (bytevector-copy a -1)
""",
        Code.RANGE_EXCEEDED );
    }

    /**
     * p50
     */
    @Test
    public void bytevector_copy_2_err_over() throws Exception
    {
        expectError(
"""
        (define a #u8(1 2 3 4 5))
        (bytevector-copy a 6)
""",
        Code.INDEX_OUT_OF_BOUNDS );
    }

    /**
     * p50
     */
    @Test
    public void bytevector_copy_3() throws Exception
    {
        expectFco(
"""
        (define a #u8(1 2 3 4 5))
        (bytevector-copy a 2 4)
""",
        parse( "#u8(3 4)" ) );
    }

    /**
     * p50
     */
    @Test
    public void bytevector_copy_3_empty_1() throws Exception
    {
        expectFco(
"""
        (define a #u8(1 2 3 4 5))
        (bytevector-copy a 5 5)
""",
        parse( "#u8()" ) );
    }

    /**
     * p50
     */
    @Test
    public void bytevector_copy_3_err_illegal() throws Exception
    {
        expectError(
"""
        (define a #u8(1 2 3 4 5))
        (bytevector-copy a 4 2)
""",
        Code.ILLEGAL_ARGUMENT );
    }

    /**
     * p50
     */
    @Test
    public void bytevector_copy_3_err_minus() throws Exception
    {
        expectError(
"""
        (define a #u8(1 2 3 4 5))
        (bytevector-copy a -1 2)
""",
        Code.RANGE_EXCEEDED );
    }

    /**
     * p50
     */
    @Test
    public void bytevector_copy_3_err_index() throws Exception
    {
        expectError(
"""
        (define a #u8(1 2 3 4 5))
        (bytevector-copy a 5 6)
""",
        Code.INDEX_OUT_OF_BOUNDS );
    }

    /**
     * p50
     */
    @Test
    public void bytevector_copyq_3() throws Exception
    {
        expectFco(
"""
        (define a #u8(1 2 3))
        (define b #u8(10 20 30 40 50))
        (bytevector-copy! b 1 a)
        b
""",
        parse( "#u8(10 1 2 3 50)" ) );
    }

    /**
     * p50
     */
    @Test
    public void bytevector_copyq_4() throws Exception
    {
        expectFco(
"""
        (define a #u8(1))
        (define b #u8(10 20 30 40 50))
        (bytevector-copy! b 1 a 0)
        b
""",
        parse( "#u8(10 1 30 40 50)" ) );
    }

    /**
     * p50
     */
    @Test
    public void bytevector_copyq_5() throws Exception
    {
        expectFco(
"""
        (define a #u8(1 2 3 4 5))
        (define b #u8(10 20 30 40 50))
        (bytevector-copy! b 1 a 0 2)
        b
""",
        parse( "#u8(10 1 2 40 50)" ) );
    }

    /**
     * p50
     */
    @Test
    public void bytevector_append() throws Exception
    {
        expectFco(
"""
        (let
          (
            (a (bytevector 0 1 2 3 4 5))
            (b (bytevector 6 7 8 9 10 11 12 13 14 15 16))
            (c (bytevector 17 18 19 20 21 22 23 24))
          )
          (bytevector-append a b c)
        )

""",
        parse( "#u8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24)" ) );
    }

    /**
     * p50
     */
    @Test
    public void bytevector_append_empty() throws Exception
    {
        expectFco(
"""
        (let
          (
            (a (bytevector))
            (b (bytevector 6 7 8 9 10 11 12 13 14 15 16))
            (c (bytevector))
          )
          (bytevector-append a b c)
        )

""",
        parse( "#u8(6 7 8 9 10 11 12 13 14 15 16)" ) );
    }

    /**
     * p50
     */
    @Test
    public void bytevector_utf8_to_string() throws Exception
    {
        expectFco(
"""
        ;                           spc1                               spc2
        (utf8->string #u8(#xce #xbb #x20 #x48 #xc3 #xa9 #x6c #xc3 #xb4 #x20 #xe4 #xb8 #xa7))
""",
        parse( "\"λ Hélô 丧\"" ) );
    }

    /**
     * p50
     */
    @Test
    public void bytevector_string_to_utf8() throws Exception
    {
        expectFco(
"""
        (string->utf8 "λ Hélô 丧")
""",
        //                    spc1                               spc2
        parse( "#u8(#xce #xbb #x20 #x48 #xc3 #xa9 #x6c #xc3 #xb4 #x20 #xe4 #xb8 #xa7)" ) );
    }
}
