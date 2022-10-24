package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import javax.script.ScriptException;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.SchemeBoolean;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamException.Code;

/**
 * rsr7 6.4 Pairs and lists.
 *
 * @author MICBINZ
 */
public class Rsr7_6_4_PairsLists extends ScreamBaseTest
{
    /**
     * p42
     */
    @Test
    public void schemeLength_1() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (length '(a b c))
                """ );
        assertEquals( i(3), result );
    }

    /**
     * p42
     */
    @Test
    public void schemeLength_2() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (length '(a (b) (c d e)))
                """ );
        assertEquals( i(3), result );
    }

    /**
     * p42
     */
    @Test
    public void schemeLength_3() throws Exception
    {
        var result = scriptEngine().evalFco(
                """
                (length '())
                """ );
        assertEquals( i(0), result );
    }

    @Test
    public void schemeLength_improperFail() throws Exception
    {
        try
        {
            scriptEngine().evalFco(
                """
                (length '(a b . c))
                """ );
            fail();
        }
        catch ( ScriptException x )
        {
            RuntimeX rx = (RuntimeX)x.getCause();
            assertEquals( Code.EXPECTED_PROPER_LIST, rx.getCode() );
        }
    }

    @Test
    public void schemeAppend_1() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (define result
                    (append '(x) '(y)))
            (equal? '(x y) result)
            """ );
        assertEquals( SchemeBoolean.T, result );
    }
}
