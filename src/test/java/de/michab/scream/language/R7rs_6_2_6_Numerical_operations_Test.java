/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.language;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;

public class R7rs_6_2_6_Numerical_operations_Test extends ScreamBaseTest
{
    /**
     * r7rs truncate 6.2.6 p37
     */
    @Test
    public void truncate_1() throws Exception
    {
        expectFco(
                "(truncate -4.3)",
                d( -4.0 ) );
    }
    /**
     * r7rs truncate 6.2.6 p37
     */
    @Test
    public void truncate_2() throws Exception
    {
        expectFco(
                "(truncate 3.5)",
                d( 3.0 ) );
    }

    @Test
    public void sqrt() throws Exception
    {
        final var SQRT2 = Math.sqrt( 2.0 );

        expectFco(
                "(sqrt 2)",
                d( SQRT2 ) );
    }
    @Test
    public void sqrt_2() throws Exception
    {
        final var SQRT2 = Math.sqrt( 2.0 );

        expectFco(
                "(sqrt 2.0)",
                d( SQRT2 ) );
    }
    @Test
    public void sqrt_3() throws Exception
    {
        expectFco(
                "(integer? (sqrt 4))",
                bTrue );
    }

    @Test
    public void exact_integer_sqrt_1() throws Exception
    {
        expectFco(
                "(exact-integer-sqrt 4)",
                "(2 0)" );
    }
    @Test
    public void exact_integer_sqrt_2() throws Exception
    {
        expectFco(
                "(exact-integer-sqrt 5)",
                "(2 1)" );
    }
    @Test
    public void exact_integer_sqrt_3() throws Exception
    {
        expectFco(
                "(exact-integer-sqrt 32)",
                "(5 7)" );
    }
}
