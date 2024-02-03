/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2023 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Environment;

/**
 * r7rs 6.12 Environment and evaluation
 *
 * @author MICBINZ
 */
public class R7rs_6_12_Environments_and_evaluation_Test extends ScreamBaseTest
{
    /**
     * p55
     */
    @Test
    public void scheme_report_environment() throws Exception
    {
        var se = scriptEngine();

        {
            var result = se.evalFco(
                """
                (scheme-report-environment 7)
                """ );
            assertInstanceOf( Environment.class, result );
            assertTrue( result.isConstant() );
        }
        {
            var result = se.evalFco(
                """
                (scheme-report-environment 6)
                """ );
            assertInstanceOf( Environment.class, result );
            assertTrue( result.isConstant() );
        }
        {
            var result = se.evalFco(
                """
                (scheme-report-environment 5)
                """ );
            assertInstanceOf( Environment.class, result );
            assertTrue( result.isConstant() );
        }
    }

    /**
     * p55
     */
    @Test
    public void scheme_report_environment_error() throws Exception
    {
        expectError( "(scheme-report-environment 4)", Code.ILLEGAL_ARGUMENT );
        expectError( "(scheme-report-environment 8)", Code.ILLEGAL_ARGUMENT );
    }

    /**
     * p55
     */
    @Test
    public void null_environment() throws Exception
    {

        var se = scriptEngine();

        {
            var result = se.evalFco(
                """
                (null-environment 7)
                """ );
            assertInstanceOf( Environment.class, result );
            assertTrue( result.isConstant() );
        }
        {
            var result = se.evalFco(
                """
                (null-environment 6)
                """ );
            assertInstanceOf( Environment.class, result );
            assertTrue( result.isConstant() );
        }
        {
            var result = se.evalFco(
                """
                (null-environment 5)
                """ );
            assertInstanceOf( Environment.class, result );
            assertTrue( result.isConstant() );
        }
    }

    /**
     * p55
     */
    @Test
    public void scheme_null_environment_error() throws Exception
    {
        expectError( "(null-environment 4)", Code.ILLEGAL_ARGUMENT );
        expectError( "(null-environment 8)", Code.ILLEGAL_ARGUMENT );
    }

    /**
     * p55
     */
    @Test
    public void interaction_environment() throws Exception
    {
        var se = scriptEngine();

        {
            var result = se.evalFco(
                """
                (interaction-environment)
                """ );
            assertInstanceOf( Environment.class, result );
            assertFalse( result.isConstant() );
        }
    }

    /**
     * p55
     */
    @Test
    public void eval1() throws Exception
    {
        expectFco(
                "(eval '(* 7 3) (scheme-report-environment 7))",
                i(21) );
    }

    /**
     * p55
     */
    @Test
    public void eval2() throws Exception
    {
        expectFco(
                """
                (let ((f (eval '(lambda (f x) (f x x))
                    (null-environment 7))))
                    (f + 10))
                """,
                i(20) );
    }

    /**
     * p55
     */
    @Test
    public void evalDef() throws Exception
    {
        var t = makeTester();

        t.execute(
                """
                (eval '(define donald 313) (interaction-environment))
                """ );
        t.expectFco(
                "donald",
                i313 );
    }

    @Test
    public void evalDefFailure() throws Exception
    {
        expectError(
                """
                (eval '(define donald 313) (scheme-report-environment 7))
                """,
                Code.CANNOT_MODIFY_CONSTANT );
    }
}
