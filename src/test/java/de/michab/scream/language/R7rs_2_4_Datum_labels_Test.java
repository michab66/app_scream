/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2024 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import java.util.HashMap;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Cons;

public class R7rs_2_4_Datum_labels_Test extends ScreamBaseTest
{
    private void addStepperEngine( Tester t ) throws RuntimeX
    {
        t.execute(
"""
(define (stepper list)
  (let ((current list))
     (lambda ()
       (let ((result (car current)))
         (set! current (cdr current))
         result))))
"""
         );
    }

    /**
     * r7rs 2.4 p9
     */
    @Test
    public void r7rs_example() throws Exception
    {
        var t = makeTester();

        t.expectFco(
"""
                (scream:circular?
                  '#0=(a b c . #0#)
                )
""",
                bTrue );
        t.expectError(
                "#0= #0#",
                Code.SYNTAX_ERROR );
        t.expectError(
                "#0=#0#",
                Code.SYNTAX_ERROR );
        t.expectError(
                "#313=#0#",
                Code.SYNTAX_ERROR );
    }

    @Test
    public void iterate_simple() throws Exception
    {
        var t = makeTester();

        addStepperEngine( t );

        t.execute(
"""
                (define circular
                  '#0=(a b c . #0#)
                )
"""
                );
        t.execute(
"""
                (define nx (stepper circular))"""
                );
        t.expectFco( "(nx)", s("a") );
        t.expectFco( "(nx)", s("b") );
        t.expectFco( "(nx)", s("c") );
        t.expectFco( "(nx)", s("a") );
    }

    @Test
    public void iterate_simple_1() throws Exception
    {
        var t = makeTester();

        addStepperEngine( t );

        t.execute(
"""
                (define circular
                  '(a . #0=(b c . #0#))
                )
"""
                );
        t.execute(
"""
                (define nx (stepper circular))"""
                );
        t.expectFco( "(nx)", s("a") );
        t.expectFco( "(nx)", s("b") );
        t.expectFco( "(nx)", s("c") );
        t.expectFco( "(nx)", s("b") );
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

    @Test
    public void toDatumLabels() throws Exception
    {
       var fco =
               parse( "(1 #0=(11 . #1=(12 . #0#)) 2 . #1#)" );
       var cons =
               assertInstanceOf( Cons.class, fco );


       var nodeMap = Cons.enumerateNodes( cons );
       assertEquals( 5, nodeMap.size() );
    }

    @Test
    public void toDatumCollections() throws Exception
    {
        var fco =
                parse( "(1 #0=(11 . #1=(12 . #0#)) 2 . #1#)" );
        var cons =
                assertInstanceOf( Cons.class, fco );


        HashMap<Cons,Long> nodes =
                new HashMap<>();
        HashMap<Cons,Long> refs =
                new HashMap<>();

        Cons.collectNodes( cons, nodes, refs );

        assertEquals(
                5,
                nodes.size() );
        assertEquals(
                2,
                refs.size() );
    }

    @Test
    public void toDatumLabelString() throws Exception
    {
        var fco =
                parse( "(1 #2=(11 . #3=(12 . #2#)) 2 . #3#)" );
        var cons =
                assertInstanceOf( Cons.class, fco );

        Cons.prepareNodes( cons );
    }
}
