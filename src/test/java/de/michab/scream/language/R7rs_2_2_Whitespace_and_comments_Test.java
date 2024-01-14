/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2024 Michael G. Binz
 */
package de.michab.scream.language;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;

public class R7rs_2_2_Whitespace_and_comments_Test extends ScreamBaseTest
{
    /**
     * r7rs 2.2 p8
     */
    @Test
    public void r7rs_example() throws Exception
    {
        expectFco(
                """
                #|
                    The FACT procedure computes the factorial
                    of a non-negative integer.
                |#
                (define fact
                  (lambda (n)
                    (if (= n 0)
                      #; (= n 1)
                      1  ; Base case: return 1
                      (* n (fact (- n 1))))))

                (fact 19)

                """,

                "121645100408832000" );
    }

    @Test
    public void nested_substructure() throws Exception
    {
        expectFco(
                """
                #| Prefix.  Nesting =>  #|
                    The FACT procedure computes the factorial
                    of a non-negative integer.
                |# <= end nesting.
                |#
                (define fact
                  #;(lambda (n) ; <= commented.
                    (if (= n 0)
                      #; (= n 1) ; <= nested.
                      1  ; Base case: return 1
                      (* n (fact (- n 1))))) 313)

                fact

                """,

                i313 );
    }
}
