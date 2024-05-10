/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2024 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.util.ConsToString;
import de.michab.scream.util.Scut;

public class R7rs_2_4_Datum_labels_Test extends ScreamBaseTest
{
    private void prefixEquals( String prefix, Cons cons ) throws RuntimeX
    {
        Symbol skip = Symbol.createObject( "*" );

        for ( var fco : Scut.as( Cons.class, parse( prefix ) ) )
        {
            if ( ! skip.eq( fco ) )
                assertEqualq( fco, cons.getCar() );

            cons = Scut.as( Cons.class, cons.getCdr() );
        }
    }

    /**
     * Checks if elements in a cons are in a certain order.
     *
     * @param cons The list that defines the elements.  This may contain
     * cycles.
     * @param list An ordered list of elements that is compared against
     * the passed cons. A symbol element '*' in this list results in a skip.
     * Recommended is that this list contains only number and skip elements.
     */
    private void testElementOrder(
            String cons,
            String list)
                    throws Exception
    {
        var c = Scut.as(
                Cons.class,
                parse( cons ) );

        var cts = new ConsToString( c ).toString();

        prefixEquals(
                list,
                Scut.as(
                        Cons.class,
                        parse( cts ) ) );
    }

    private void checkConversion( String cons ) throws Exception
    {
        var expected = cons;

        var intermediate = Scut.as(
                Cons.class,
                parse( expected ) );

        var cts = new ConsToString( intermediate ).toString();

        assertEquals( expected, cts );
    }

    private void testLabelAdjustment( String in, String out ) throws Exception
    {
        var cons = assertInstanceOf(
                Cons.class,
                parse( in ) );

        assertEquals(
                out,
                new ConsToString( cons ).toString() );
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
    public void conversionRoundtrip() throws Exception
    {
        checkConversion(
                "()" );
        checkConversion(
                "(m i c b i n z)" );
        checkConversion(
                "((m 1) (i 2) (c 3) (b 4) (i 5) (n 6) (lz 7))" );
        checkConversion(
                "(m i c b i n z . 313)" );
        checkConversion(
                "#0=(a b c . #0#)" );
        checkConversion(
                "(1 #0=(11 . #1=(12 . #0#)) 2 . #1#)" );

//        checkConversion(
//                "#( #1=(1 2) #2=(10 20 . #2#) #1# #1# )" );
    }

    @Test
    public void testOrders() throws Exception
    {
        testElementOrder(
                "(m i c b i n z)",
                "(m i c b i n z)" );

        testElementOrder(
                "#0=(a b c . #0#)",
                "(a b c a)" );

        testElementOrder(
                "(a . #0=(b c . #0#))",
                "(a b c b c)" );

        testElementOrder(
                "(1 #2=(11 . #3=(12 . #2#)) 2 . #3#)",
                "(1 * 2 12 11 12 11)" );
    }

    @Test
    public void labelAdjustment0() throws Exception
    {
        testLabelAdjustment(
                // Labels 2 3 ...
                "(1 #2=(11 . #3=(12 . #2#)) 2 . #3#)",
                // ... 0 1
                "(1 #0=(11 . #1=(12 . #0#)) 2 . #1#)" );

        testLabelAdjustment(
                "(1 #313=(11 . #0=(12 . #313#)) 2 . #0#)",
                "(1 #0=(11 . #1=(12 . #0#)) 2 . #1#)" );

        testLabelAdjustment(
                "(a . #9=(b c . #9#))",
                "(a . #0=(b c . #0#))" );

        testLabelAdjustment(
                "#1=(a b c . #1#)",
                "#0=(a b c . #0#)" );
    }

    /**
     * r7rs 2.4 p9
     */
    @Test
    @Disabled
    public void programReferences() throws Exception
    {
        makeTester().execute(
                "#1=(begin (display 0) #1#)" );
    }
}
