/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2024 Michael G. Binz
 */
package de.michab.scream.language;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Cons;

/**
 * scream extensions
 */
public class Scream_Java_binding_Test extends ScreamBaseTest
{
    @Test
    public void cons_s_scream$circularq()
            throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(define circular0 (scream:make-circular! (list 1)))",
                Cons.NIL );
        t.expectFco(
                "(scream:circular? circular0)",
                bTrue );
        t.expectFco(
                "(define circular1 (scream:make-circular! (list 2)))",
                Cons.NIL );
        t.expectFco(
                "(scream:circular? circular0 circular1)",
                bTrue );
        t.expectFco(
                "(scream:circular? circular0 circular1 (list 3))",
                bFalse );
    }
}
