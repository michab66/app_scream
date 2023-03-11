/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.pops;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamEvaluator;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Operation;

public class SyntaxSetTest extends ScreamBaseTest
{
    @Test
    public void exists() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                set!
                """ );
        assertInstanceOf( Operation.class, result );
    }

    @Test
    public void assign() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var tle = se.getInteraction();

        tle.define( s313, i1 );

        var result = se.evalFco(
                """
                (set! threethirteen 313)
                """ );
        assertEquals( Cons.NIL, result );
        assertEquals( i313, tle.get( s313 ) );
    }

    @Test
    public void assign2() throws RuntimeX {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (define x 0)
                (set! x 313)
                x
                """ );
        assertEquals( ScreamBaseTest.i313, result );
    }
}
