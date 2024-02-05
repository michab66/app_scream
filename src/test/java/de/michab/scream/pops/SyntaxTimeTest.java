/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.pops;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamEvaluator;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Operation;
import de.michab.scream.fcos.Int;

public class SyntaxTimeTest extends ScreamBaseTest
{
    @Test
    public void exists() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco( "%time" );
        assertInstanceOf( Operation.class, result );
    }

    private void validateResult( FirstClassObject result )
    {
        Cons cons = (Cons)result;
        assertFalse( ((Cons)result).isProperList() );
        Int car = (Int)cons.getCar();
        Int cdr = (Int)cons.getCdr();

        assertTrue( 3 == cdr.asLong() );
        assertTrue( car.asLong() >= 0 );
    }

    @Test
    void time_1() throws Exception
    {
        var result = scriptEngine().evalFco(
                "(%time (+ 1 2))" );

        validateResult( result );
    }
}
