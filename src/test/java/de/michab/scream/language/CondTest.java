/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */

package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.logging.Logger;

import org.junit.jupiter.api.Test;

import de.michab.scream.Cons;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamEvaluator;

/**
 * r7, 4.2.1 Conditionals, cond
 */
public class CondTest extends ScreamBaseTest
{
    private static Logger LOG = Logger.getLogger( CondTest.class.getName() );

    /**
     * No else clause, no clause applies.
     */
    @Test
    public void condTest3() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (cond ((> 3 3) 'greater)
                      ((< 3 3) 'less))
                """ );
        assertEquals( Cons.NIL, result );
    }

    /**
     * rsr7: If the selected <clause> contains only the <test> and no
     * <expression>s, then the value of the <test> is returned as
     * the result.
     */
    @Test
    public void condTest() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (cond ((> 2 3) 'greater)
                      ((+ 2 3)))
                """ );
        assertEquals( result, i( 5 ) );
    }

    /**
     * rsr7: If the selected <clause> contains only the <test> and no
     * <expression>s, then the value of the <test> is returned as
     * the result.
     */
    @Test
    public void _condTest2() throws Exception
    {
        _contTest(
                """
                (cond ((> 2 3) 'greater)
                      ((+ 2 3)))
                """,
                i(5)
                );
    }
}
