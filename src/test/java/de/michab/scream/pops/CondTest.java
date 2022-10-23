/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */

package de.michab.scream.pops;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.logging.Logger;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import de.michab.scream.Cons;
import de.michab.scream.SchemeEvaluator2;
import de.michab.scream.SchemeInterpreter2;
import de.michab.scream.TestUtil;

/**
 * r7, 4.2.1 Conditionals, cond
 */
public class CondTest extends TestUtil
{
    private static Logger LOG = Logger.getLogger( CondTest.class.getName() );

    /**
     * No else clause, one clause applies.
     */
    @Test
    public void condTest1() throws Exception
    {
        SchemeEvaluator2 se = (SchemeEvaluator2)new SchemeInterpreter2().getScriptEngine();

        var result = se.eval(
                """
                (cond ((> 3 2) 'greater)
                      ((< 3 2) 'less))
                """ );
        assertEquals( s( "greater" ), result );
    }

    /**
     * With else.
     */
    @Test
    public void condTest2() throws Exception
    {
        SchemeEvaluator2 se = (SchemeEvaluator2)new SchemeInterpreter2().getScriptEngine();

        var result = se.eval(
                """
                (cond ((> 3 3) 'greater)
                      ((< 3 3) 'less)
                      (else 'equal))
                """ );
        assertEquals( s( "equal" ), result );
    }

    /**
     * No else clause, no clause applies.
     */
    @Test
    public void condTest3() throws Exception
    {
        SchemeEvaluator2 se = (SchemeEvaluator2)new SchemeInterpreter2().getScriptEngine();

        var result = se.eval(
                """
                (cond ((> 3 3) 'greater)
                      ((< 3 3) 'less))
                """ );
        assertEquals( Cons.NIL, result );
    }

    /**
     * r7, 4.2.1 Conditionals, cond
     */
    @Disabled // not yet supported
    @Test
    public void condTest4() throws Exception
    {
        SchemeEvaluator2 se = (SchemeEvaluator2)new SchemeInterpreter2().getScriptEngine();

        var result = se.eval(
                """
                (cond ((assv 'b '((a 1) (b 2))) => cadr)
                      (else #f))
                """ );
        assertEquals( result, i2 );
    }

    /**
     * rsr7: If the selected <clause> contains only the <test> and no
     * <expression>s, then the value of the <test> is returned as
     * the result.
     */
    @Test
    public void condTest() throws Exception
    {
        SchemeEvaluator2 se = (SchemeEvaluator2)new SchemeInterpreter2().getScriptEngine();

        var result = se.eval(
                """
                (cond ((> 2 3) 'greater)
                      ((+ 2 3)))
                """ );
        assertEquals( result, i( 5 ) );
    }

    @Test
    public void _condTest1() throws Exception
    {
        _contTest(
                """
                (cond ((> 2 3) 'greater)
                      ((< 2 3) 'less))
                """,
                s("less") );
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
