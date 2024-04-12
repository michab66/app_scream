/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2024 Michael G. Binz
 */
package de.michab.scream.language;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Bool;

/**
 * rsr7 6.5 Symbols, p43
 *
 * @author MICBINZ
 */
public class R7rs_6_5_Symbols_Test extends ScreamBaseTest
{
    /**
     * p44
     */
    @Test
    public void symbolq() throws Exception
    {
        var t = makeTester();

        t.expectFco( "(symbol? 'donald)", Bool.T );
        t.expectFco( "(symbol? 313)", Bool.F );
    }

    /**
     * p44
     */
    @Test
    public void symbolToString() throws Exception
    {
        var t = makeTester();

        t.expectFco( "(symbol->string 'donald)", str( "donald" ) );
    }

    /**
     * p44
     */
    @Test
    public void stringToSymbol() throws Exception
    {
        var t = makeTester();

        t.expectFco( "(string->symbol \"donald\")", s( "donald" ) );
    }
}
