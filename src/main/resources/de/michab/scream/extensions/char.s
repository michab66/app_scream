;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 1998-2024 Michael G. Binz
;

;;
;; Scream definitions.
;;

(define scream:type-character
  ((make-object "de.michab.scream.fcos.SchemeCharacter") "TYPE_NAME"))

;;
;; (char? obj) procedure; r5rs 29
;;
(define char?
  (typePredicateGenerator "de.michab.scream.fcos.SchemeCharacter" #t))



;;
;; (char->integer char) procedure; r5rs 29
;;
(define (char->integer char)
  (if (char? char)
    ((object char) ("asInteger"))
    (error "TYPE_ERROR" %type-char (scream:typename char))))



;;
;; (integer->char n) procedure; r5rs 29
;;
(define (integer->char i)
  (if (integer? i)
    ((make-object "de.michab.scream.SchemeCharacter") (createObject i))
    (error "TYPE_ERROR" %type-integer (scream:typename i))))



;;
;; Comparison procedures.  No locale information is used, just as
;; specified in r5rs p. 29
;; The case independent comparison procedures are after the central let block...
;;
(define (char=? c1 c2)
  (eqv? c1 c2))
(set! char=? (transitiveBoolean char=?))

(define (char<? c1 c2)
  (< (char->integer c1) (char->integer c2)))
(set! char<? (transitiveBoolean char<?))

(define (char>? c1 c2)
  (> (char->integer c1) (char->integer c2)))
(set! char>? (transitiveBoolean char>?))

(define (char<=? c1 c2)
  (<= (char->integer c1) (char->integer c2)))
(set! char<=? (transitiveBoolean char<=?))

(define (char>=? c1 c2)
  (>= (char->integer c1) (char->integer c2)))
(set! char>=? (transitiveBoolean char>=?))


;;
;; Set up dummy bindings in the global TLE.  The actual method definitions
;; will be done in the following let expression.
;;
(define char-alphabetic? ())
(define char-numeric? ())
(define char-whitespace? ())
(define char-upper-case? ())
(define char-lower-case? ())
(define char-upcase ())
(define char-downcase ())

;;
;; Open a special scope to keep the java.lang.Character instance in the local closures.
;; Replace the TLE bindings with the function implementations in the let expression.
;;
(let ((char (make-object "java.lang.Character")))

  ;;
  ;; char-alphabetic? - library procedure - r5rs p. 29
  ;;
  (set! char-alphabetic? (lambda (x) (char (isLetter x))))

  ;;
  ;; char-numeric? - library procedure - r5rs p. 29
  ;;
  (set! char-numeric? (lambda (x) (char (isDigit x))))

  ;;
  ;; char-whitespace? - library procedure - r5rs p. 29
  ;;
  (set! char-whitespace? (lambda (x) (char (isWhitespace x))))

  ;;
  ;; char-upper-case? - library procedure - r5rs p. 29
  ;;
  (set! char-upper-case? (lambda (x) (char (isUpperCase x))))

  ;;
  ;; char-lower-case? - library procedure - r5rs p. 29
  ;;
  (set! char-lower-case? (lambda (x) (char (isLowerCase x))))

  ;;
  ;; char-upcase - library procedure - r5rs p. 29
  ;;
  (set! char-upcase (lambda (x) (char (toUpperCase x))))

  ;;
  ;; char-downcase - library procedure - r5rs p. 29
  ;;
  (set! char-downcase (lambda (x) (char (toLowerCase x))))

)

;;
;; Case independent comparison procedures.  No locale information is used, just
;; as specified in r5rs p. 29
;;
(define (char-ci=? c1 c2)
  (eqv? (char-upcase c1)
        (char-upcase c2)))
(set! char-ci=? (transitiveBoolean char-ci=?))


(define (char-ci<? c1 c2)
  (< (char->integer (char-upcase c1))
     (char->integer (char-upcase c2))))
(set! char-ci<? (transitiveBoolean char-ci<?))


(define (char-ci>? c1 c2)
  (> (char->integer (char-upcase c1))
     (char->integer (char-upcase c2))))
(set! char-ci>? (transitiveBoolean char-ci>?))


(define (char-ci<=? c1 c2)
  (<= (char->integer (char-upcase c1))
      (char->integer (char-upcase c2))))
(set! char-ci<=? (transitiveBoolean char-ci<=?))


(define (char-ci>=? c1 c2)
  (>= (char->integer (char-upcase c1))
      (char->integer (char-upcase c2))))
(set! char-ci>=? (transitiveBoolean char-ci>=?))
