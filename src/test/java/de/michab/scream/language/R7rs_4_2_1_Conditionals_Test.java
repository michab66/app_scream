package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;

/**
 * r7rs 4.2.1 Conditionals.
 *
 * @author MICBINZ
 */
public class R7rs_4_2_1_Conditionals_Test extends ScreamBaseTest
{
    /**
     * p14
     */
    @Test
    public void cond_1() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (cond ((> 3 2) 'greater)
                  ((< 3 2) 'less))
            """ );
        assertEquals( s("greater"), result );
    }

    /**
     * p14
     */
    @Test
    public void cond_2() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (cond ((> 3 3) 'greater)
                  ((< 3 3) 'less)
                  (else 'equal))
            """ );
        assertEquals( s("equal"), result );
    }

    /**
     * p14
     */
    @Disabled( "not implemented." )
    @Test
    public void cond_3() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (cond ((assv 'b '((a 1) (b 2))) => cadr)
                  (else #f))
            """ );
        assertEquals( i(2), result );
    }
}
