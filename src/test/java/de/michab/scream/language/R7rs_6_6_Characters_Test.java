/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2024 Michael G. Binz
 */
package de.michab.scream.language;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;

/**
 * rsr7 6.6 Characters, p44
 *
 * @author MICBINZ
 */
public class R7rs_6_6_Characters_Test extends ScreamBaseTest
{
    /**
     * p44
     */
    @Test
    public void charp_1() throws Exception
    {
        expectFco(
            """
            (char? #\\ä)
            """,
            bTrue );
    }

//    ;;
//    ;; Test (char? obj)
//    ;;
//    (%positive-test sourcefile 1
//      (char? #\ä)
//      #t)
//
//    ;;
//    ;; Test (char->integer char) procedure; r5rs 29
//    ;;
//    (%positive-test sourcefile 2
//      (char->integer #\@)
//      64)
//
//    ;;
//    ;; Test (integer->char n) procedure; r5rs 29
//    ;;
//    (%positive-test sourcefile 3
//      (integer->char 64)
//      #\@)
//
//    ;;
//    ;; Test char=?
//    ;;
//    (%positive-test sourcefile 4
//      (char=? #\ä #\ä)
//      #t)
//    (%positive-test sourcefile 5
//      (char=? #\ä #\ö)
//      #f)
//
//    (%positive-test sourcefile 6
//      (char<? #\a #\z)
//      #t)
//    (%positive-test sourcefile 7
//      (char<? #\a #\b #\z)
//      #t)
//    (%positive-test sourcefile 8
//      (char<? #\b #\a)
//      #f)
//    (%positive-test sourcefile 9
//      (char<? #\a #\c #\b)
//      #f)
//
//    (%positive-test sourcefile 10
//      (char>? #\z #\a)
//      #t)
//    (%positive-test sourcefile 11
//      (char>? #\z #\x #\a)
//      #t)
//    (%positive-test sourcefile 12
//      (char>? #\a #\b)
//      #f)
//    (%positive-test sourcefile 13
//      (char>? #\z #\w #\x)
//      #f)
//
//    (%positive-test sourcefile 14
//      (char<=? #\a #\z)
//      #t)
//    (%positive-test sourcefile 15
//      (char<=? #\a #\m #\z)
//      #t)
//    (%positive-test sourcefile 16
//      (char<=? #\X #\A)
//      #f)
//    (%positive-test sourcefile 17
//      (char<=? #\a #\z #\y)
//      #f)
//
//    ;; Holy cow, how boring...
//    ;;(define (char>=? c1 c2)
//    ;;  (>= (char->integer c1) (char->integer c2)))
//    ;;(set! char>=? (transitiveBoolean char>=?))
//
//
//    ;;
//    ;; Set up dummy bindings in the global TLE.  The actual method definitions
//    ;; will be done in the following let expression.
//    ;;
//    ;;(define char-alphabetic? ())
//    ;;(define char-numeric? ())
//    ;;(define char-whitespace? ())
//    ;;(define char-upper-case? ())
//    ;;(define char-lower-case? ())
//    ;;(define char-upcase ())
//    ;;(define char-downcase ())
//
//    ;;
//    ;; Open a special scope to keep the java.lang.Character instance in the local closures.
//    ;; Replace the TLE bindings with the function implementations in the let expression.
//    ;;
//    ;;(let ((char (make-object java.lang.Character)))
//
//      ;;
//      ;; char-alphabetic? - library procedure - r5rs p. 29
//      ;;
//      ;;(set! char-alphabetic? (lambda (x) (char (isLetter x))))
//
//      ;;
//      ;; char-numeric? - library procedure - r5rs p. 29
//      ;;
//      ;;(set! char-numeric? (lambda (x) (char (isDigit x))))
//
//      ;;
//      ;; char-whitespace? - library procedure - r5rs p. 29
//      ;;
//      ;;(set! char-whitespace? (lambda (x) (char (isWhitespace x))))
//
//      ;;
//      ;; char-upper-case? - library procedure - r5rs p. 29
//      ;;
//      ;;(set! char-upper-case? (lambda (x) (char (isUpperCase x))))
//
//      ;;
//      ;; char-lower-case? - library procedure - r5rs p. 29
//      ;;
//      ;;(set! char-lower-case? (lambda (x) (char (isLowerCase x))))
//
//      ;;
//      ;; char-upcase - library procedure - r5rs p. 29
//      ;;
//      ;;(set! char-upcase (lambda (x) (char (toUpperCase x))))
//
//      ;;
//      ;; char-downcase - library procedure - r5rs p. 29
//      ;;
//      ;;(set! char-downcase (lambda (x) (char (toLowerCase x))))
//
//    ;;)
//
//    ;;
//    ;; Case independent comparison procedures.  No locale information is used, just
//    ;; as specified in r5rs p. 29
//    ;;
//    ;;(define (char-ci=? c1 c2)
//    ;;  (eqv? (char-upcase c1)
//    ;;        (char-upcase c2)))
//    ;;(set! char-ci=? (transitiveBoolean char-ci=?))
//
//
//    ;;(define (char-ci<? c1 c2)
//    ;;  (< (char->integer (char-upcase c1))
//    ;;     (char->integer (char-upcase c2))))
//    ;;(set! char-ci<? (transitiveBoolean char-ci<?))
//
//
//    ;;(define (char-ci>? c1 c2)
//    ;;  (> (char->integer (char-upcase c1))
//    ;;     (char->integer (char-upcase c2))))
//    ;;(set! char-ci>? (transitiveBoolean char-ci>?))
//
//
//    ;;(define (char-ci<=? c1 c2)
//    ;;  (<= (char->integer (char-upcase c1))
//    ;;      (char->integer (char-upcase c2))))
//    ;;(set! char-ci<=? (transitiveBoolean char-ci<=?))
//
//
//    ;;(define (char-ci>=? c1 c2)
//    ;;  (>= (char->integer (char-upcase c1))
//    ;;      (char->integer (char-upcase c2))))
//    ;;(set! char-ci>=? (transitiveBoolean char-ci>=?))
}
