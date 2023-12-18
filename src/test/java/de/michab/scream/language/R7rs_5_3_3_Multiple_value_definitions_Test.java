/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.language;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;

/**
 * r7rs 5.3.3 Multiple-value definitions
 *
 * @author micbinz
 */
public class R7rs_5_3_3_Multiple_value_definitions_Test
    extends ScreamBaseTest
{
    /**
     * p26
     */
    @Test
    public void r7rs_example_1() throws Exception
    {
        expectFco(
        """
        (define-values (a b) (values 1 2))
        (list a b)
        """,
        "(1 2)" );
    }

    /**
     * p26
     */
    @Test
    public void r7rs_example_2() throws Exception
    {
        expectFco(
        """
        (define-values (x y) (values 1 2))
        (+ x y)
        """,
        i(3) );
    }

    /**
     * p26
     */
    @Test
    public void r7rs_single_value_1() throws Exception
    {
        expectFco(
        """
        (define-values (x) 313)
        x
        """,
        i(313) );
    }

    /**
     * p26
     */
    @Test
    public void r7rs_single_value_2() throws Exception
    {
        expectFco(
        """
        (define-values (x) (values 313))
        x
        """,
        i(313) );
    }

    /**
     * p26
     */
    @Test
    public void r7rs_mismatch_less() throws Exception
    {
        expectError(
        "(define-values (x y) (values 1))",
        // Expected two, received one.
        Code.WRONG_NUMBER_OF_ARGUMENTS );
    }

    /**
     * p26
     */
    @Test
    public void r7rs_mismatch_exceeds() throws Exception
    {
        expectError(
        "(define-values (x y) (values 1 2 3))",
        // Expected two, received three.
        Code.WRONG_NUMBER_OF_ARGUMENTS );
    }
}
