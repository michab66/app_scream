/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.language;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;

public class R7rs_6_2_6_Numerical_operations extends ScreamBaseTest
{
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
                "(integer? (sqrt 43))",
                bTrue );
    }
}
