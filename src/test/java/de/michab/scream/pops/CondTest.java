package de.michab.scream.pops;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import de.michab.scream.SchemeEvaluator2;
import de.michab.scream.SchemeInterpreter2;
import de.michab.scream.Symbol;
import de.michab.scream.TestUtil;

public class CondTest
{
    /**
     * r7, 4.2.1 Conditionals, cond
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
        assertEquals( result, Symbol.createObject( "greater" ) );
    }

    /**
     * r7, 4.2.1 Conditionals, cond
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
        assertEquals( result, Symbol.createObject( "equal" ) );
    }

    /**
     * r7, 4.2.1 Conditionals, cond
     */
    @Disabled // not yet supported
    @Test
    public void condTest3() throws Exception
    {
        SchemeEvaluator2 se = (SchemeEvaluator2)new SchemeInterpreter2().getScriptEngine();

        var result = se.eval(
                """
                (cond ((assv 'b '((a 1) (b 2))) => cadr)
                      (else #f))
                """ );
        assertEquals( result, TestUtil.i2 );
    }
}
