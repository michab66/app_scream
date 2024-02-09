/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2023 Michael G. Binz
 */
package de.michab.scream.language;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Bool;

/**
 * rsr7 6.14 System interface, p60
 *
 * @author micbinz
 */
public class R7rs_6_14_System_interface_Test extends ScreamBaseTest
{
    @Test
    public void current_second() throws Exception
    {
        var t = makeTester();

        t.execute(
                "(define t0 (current-second))" );
        t.expectFco(
                "(exact? t0)",
                Bool.F );

        Thread.sleep( 10 );
        t.execute( "(define t1 (current-second))" );
        t.expectFco(
                "(< t0 t1)",
                Bool.T );
    }

    @Test
    public void current_jiffy() throws Exception
    {
        var t = makeTester();

        t.execute(
                "(define t0 (current-jiffy))" );
        t.expectFco(
                "(exact? t0)",
                Bool.T );

        Thread.sleep( 10 );
        t.execute( "(define t1 (current-jiffy))" );

        t.expectFco(
                "(< t0 t1)",
                Bool.T );
    }

    @Test
    public void jiffies_per_second() throws Exception
    {
        expectFco(
                "(jiffies-per-second)",
                i(1000) );
    }
}
