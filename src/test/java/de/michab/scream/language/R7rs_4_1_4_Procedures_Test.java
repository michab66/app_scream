/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Operation;

/**
 * r7rs 4.1.4 Procedures.
 *
 * @author MICBINZ
 */
public class R7rs_4_1_4_Procedures_Test extends ScreamBaseTest
{
    /**
     * p13
     */
    @Test
    public void lambda_1() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (lambda (x) (+ x x))
            """ );
        assertInstanceOf( Operation.class, result );
    }

    /**
     * p13
     */
    @Test
    public void lambda_2() throws Exception
    {
        expectFco(
                "((lambda (x) (+ x x)) 4)",
                i(8) );
    }

    /**
     * p13
     */
    @Test
    public void lambda_err() throws Exception
    {
        expectError(
                "((lambda (x) (+ x x)) 1 2)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
    }

    /**
     * p13
     */
    @Test
    public void lambda_3() throws Exception
    {
        expectFco(
            """
            (define reverse-subtract
             (lambda (x y) (- y x)))
            (reverse-subtract 7 10)
            """,
            i(3) );
    }

    /**
     * p13
     */
    @Test
    public void lambda_4() throws Exception
    {
        expectFco(
            """
            (define add4
              (let ((x 4))
                (lambda (y) (+ x y))))
            (add4 6)
            """,
            i(10) );
    }

    /**
     * p13
     */
    @Test
    public void lambda_5() throws Exception
    {
        expectFco(
            """
            ((lambda x x) 3 4 5 6)
            """,
            parse( "(3 4 5 6)" ) );
    }

    /**
     * p13
     */
    @Test
    public void lambda_6() throws Exception
    {
        expectFco(
            """
            ((lambda (x y . z) z)
             3 4 5 6)
            """,
            parse( "(5 6)" ) );
    }

    /**
     * p13
     */
    @Test
    public void lambda_x_1() throws Exception
    {
        expectError(
                """
                ((lambda (x y z x) z)
                3 4 5 6)
                """,
                Code.DUPLICATE_FORMAL );
    }

    /**
     * Test lambda empty parameter list.
     *
     * r7rs 4.1.4 p13
     */
    @Test
    public void lambda_x_2() throws Exception
    {
        expectFco(
                "((lambda () 121))",
                i(121) );
    }

    /**
     * Test lambda parameter list.
     *
     * r7rs 4.1.4 p13
     */
    @Test
    public void lambda_x_3() throws Exception
    {
        expectError(
                """
                ((lambda "kong" 121))
                """,
                Code.INVALID_FORMALS );
    }
}
