/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import de.michab.scream.Cons;
import de.michab.scream.Operation;
import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamException.Code;

/**
 * r7rs 4.1.4 Procedures.
 *
 * @author MICBINZ
 */
public class R7rs_4_1_4_Pocedures_Test extends ScreamBaseTest
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
        var result = scriptEngine().evalFco(
            """
            ((lambda (x) (+ x x)) 4)
            """ );
        assertEquals( i(8), result );
    }

    /**
     * p13
     */
    @Disabled( "to be fixed." )
    @Test
    public void lambda_err() throws Exception
    {
        try
        {
            scriptEngine().evalFco(
                """
                (lambda (x) (+ x x) 4)
                """ );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.TOO_MANY_ARGUMENTS, rx.getCode() );
        }
    }

    /**
     * p13
     */
    @Test
    public void lambda_3() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (define reverse-subtract
             (lambda (x y) (- y x)))
            (reverse-subtract 7 10)
            """ );
        assertEquals( i(3), result );
    }

    /**
     * p13
     */
    @Test
    public void lambda_4() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (define add4
              (let ((x 4))
                (lambda (y) (+ x y))))
            (add4 6)
            """ );
        assertEquals( i(10), result );
    }

    /**
     * p13
     */
    @Test
    public void lambda_5() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            ((lambda x x) 3 4 5 6)
            """ );
        var expected = readSingleExpression(
                "(3 4 5 6)", Cons.class );

        assertEqualq( expected, result );
    }

    /**
     * p13
     */
    @Test
    public void lambda_x_1() throws Exception
    {
        try
        {
            scriptEngine().evalFco(
                """
                ((lambda (x y z x) z)
                3 4 5 6)
                """ );
        }
        catch (RuntimeX rx) {
            assertEquals( Code.DUPLICATE_FORMAL, rx.getCode() );
        }
    }

    /**
     * p13
     */
    @Test
    public void lambda_6() throws Exception
    {
        var expected = readSingleExpression( "(5 6)", Cons.class );
        var result = scriptEngine().evalFco(
            """
            ((lambda (x y . z) z)
             3 4 5 6)
            """ );

        assertEqualq( expected, result );
    }

}
