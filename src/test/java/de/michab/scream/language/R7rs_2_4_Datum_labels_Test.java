/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2024 Michael G. Binz
 */
package de.michab.scream.language;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;

public class R7rs_2_4_Datum_labels_Test extends ScreamBaseTest
{
    /**
     * r7rs 2.4 p9
     */
    @Test
    public void r7rs_example() throws Exception
    {
        expectFco(
"""
                (scream:circular?
                  '#0=(a b c . #0#)
                )
""",
                bTrue );
    }

    @Test
    public void iterate_simple() throws Exception
    {
        var t = makeTester();

        t.execute(
"""
                (define circular
                  '#0=(a b c . #0#)
                )
"""
         );
    }

    /**
     * Test from https://docs.scheme.org/surveys/datum-labels/
     */
    @Test
    public void schemeOrgSurveysDatumLabels() throws Exception
    {
        expectFco(
                "(let ((x '(#1=(a b) #1#))) (eq? (car x) (cadr x)))",
                bTrue );
    }
}
