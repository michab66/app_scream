package de.michab.scream.pops;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.logging.Logger;

import org.junit.jupiter.api.Test;

import de.michab.scream.SchemeEvaluator2;
import de.michab.scream.SchemeInterpreter2;
import de.michab.scream.TestUtil;
import de.michab.scream.UrschleimTest;

public class IfTest
{
    private static Logger LOG = Logger.getLogger( UrschleimTest.class.getName() );

    @Test
    public void ifTest() throws Exception
    {
        SchemeEvaluator2 se = (SchemeEvaluator2)new SchemeInterpreter2().getScriptEngine();

        var result = se.eval(
                """
                (if #t 313 0)
                """ );
        assertEquals( result, TestUtil.i313 );
    }
}
