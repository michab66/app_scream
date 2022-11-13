package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;

/**
 * r7rs 4.2.4 Iteration.
 *
 * @author MICBINZ
 */
public class R7rs_4_2_4_Iteration_Test extends ScreamBaseTest
{
    /**
     * p18
     */
    @Test
    public void let_1() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (do ((vec (make-vector 5))
                 (i 0 (+ i 1)))
                ((= i 5) vec)
              (vector-set! vec i i))
            """ );
        assertEqualq(
                parse("#(0 1 2 3 4)"),
                result );
    }

    /**
     * p18
     */
    @Test
    public void let_1x() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (do ((vec (make-vector 7))
                 (i 0 (+ i 1)))
                ((= i (vector-length vec)) vec)
              (vector-set! vec i i))
            """ );
        assertEqualq(
                parse("#(0 1 2 3 4 5 6)"),
                result );
    }

    /**
     * p18
     */
    @Test
    public void let_2() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (let ((x '(1 3 5 7 9)))
              (do ((x x (cdr x))
                   (sum 0 (+ sum (car x))))
                  ((null? x) sum)))
                        """ );
        assertEquals( i(25), result );
    }

}
