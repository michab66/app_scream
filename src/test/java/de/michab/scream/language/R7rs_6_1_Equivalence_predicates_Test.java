/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2024 Michael G. Binz
 */
package de.michab.scream.language;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;

/**
 * rsr7 6.1 Equivalence predicates, p30
 *
 * @author MICBINZ
 */
public class R7rs_6_1_Equivalence_predicates_Test extends ScreamBaseTest
{
    /**
     * p30
     */
    @Test
    public void eqvq() throws Exception
    {
        var ts = makeTester();

        ts.expectError( "(eqv?)", Code.WRONG_NUMBER_OF_ARGUMENTS );
        ts.expectError( "(eqv? 1)", Code.WRONG_NUMBER_OF_ARGUMENTS );
        ts.expectError( "(eqv? 1 2 3)", Code.WRONG_NUMBER_OF_ARGUMENTS );

        ts.expectFco(
                "(eqv? 'a 'a)",
                bTrue );
        ts.expectFco(
                "(eqv? 'a 'b)",
                bFalse );
        ts.expectFco(
                "(eqv? 2 2)",
                bTrue );
        ts.expectFco(
                "(eqv? 2 2.0)",
                bFalse );
        ts.expectFco(
                "(eqv? '() '())",
                bTrue );
        ts.expectFco(
                "(eqv? 100000000 100000000)",
                bTrue );
//        ts.expectFco(
//                "(eqv? 0.0 +nan.0)",
//                bFalse );
        ts.expectFco(
                "(eqv? (cons 1 2) (cons 1 2))",
                bFalse );
        ts.expectFco(
                "(eqv? (lambda () 1) (lambda () 2))",
                bFalse );
        ts.expectFco(
                """
                (let ((p (lambda (x) x)))
                  (eqv? p p))
                """,
                bTrue );
        ts.expectFco(
                "(eqv? '() 'nil)",
                bFalse );

        ts.expectFco(
                "(eqv? '(a (b) c) '(a (b) c))",
                bFalse );
    }

    @Test
    public void eqvq_local_state() throws Exception
    {
        var ts = makeTester();

        ts.execute(
                """
                (define gen-counter
                  (lambda ()
                    (let ((n 0))
                      (lambda () (set! n (+ n 1)) n))))
                """ );

        ts.expectFco(
                """
                (let ((g (gen-counter)))
                  (eqv? g g))
                """,
                bTrue );

        ts.expectFco(
                """
                (eqv? (gen-counter) (gen-counter))
                """,
                bFalse );
    }

    @Test
    public void eqvq_constants() throws Exception
    {
        expectFco(
                """
                (let ((x '(a)))
                  (eqv? x x))
                """,
                bTrue );
    }

    @Test
    public void eqq() throws Exception
    {
        var ts = makeTester();

        ts.expectError( "(eq?)", Code.WRONG_NUMBER_OF_ARGUMENTS );
        ts.expectError( "(eq? 1)", Code.WRONG_NUMBER_OF_ARGUMENTS );
        ts.expectError( "(eq? 1 2 3)", Code.WRONG_NUMBER_OF_ARGUMENTS );

        ts.expectFco(
                "(eq? 'a 'a)",
                bTrue );
        ts.expectFco(
                "(eq? (list 'a)(list 'a))",
                bFalse );
        ts.expectFco(
                "(eq? '() '())",
                bTrue );
        ts.expectFco(
                "(eq? car car)",
                bTrue );
        expectFco(
                """
                (let ((x '(a)))
                  (eq? x x))
                """,
                bTrue );
        expectFco(
                """
                (let ((x '#()))
                  (eq? x x))
                """,
                bTrue );
        expectFco(
                """
                (let ((p (lambda (x) x)))
                  (eq? p p))
                """,
                bTrue );

        ts.expectFco(
                "(eq? '(a (b) c) '(a (b) c))",
                bFalse );
    }

    /**
     * p32
     */
    @Test
    public void equalq() throws Exception
    {
        var ts = makeTester();

        ts.expectError( "(equal?)", Code.WRONG_NUMBER_OF_ARGUMENTS );
        ts.expectError( "(equal? 1)", Code.WRONG_NUMBER_OF_ARGUMENTS );
        ts.expectError( "(equal? 1 2 3)", Code.WRONG_NUMBER_OF_ARGUMENTS );

        ts.expectFco(
                "(equal? 'a 'a)",
                bTrue );
        ts.expectFco(
                "(equal? '(a) '(a))",
                bTrue );
        ts.expectFco(
                "(equal? '(a (b) c) '(a (b) c))",
                bTrue );
        ts.expectFco(
                "(equal? \"abc\" \"abc\")",
                bTrue );
        ts.expectFco(
                "(equal? 2 2)",
                bTrue );
        expectFco(
                """
                  (equal?
                    (make-vector 5 'a)
                    (make-vector 5 'a))
                """,
                bTrue );
    }
}
