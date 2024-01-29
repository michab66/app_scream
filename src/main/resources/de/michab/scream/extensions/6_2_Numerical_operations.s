; Scream @ https://github.com/urschleim/scream
;
; Copyright © 1998-2024 Michael G. Binz

;;
;; r7rs 6.2.6 Numerical operations, p35
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scream definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define scream:type-integer
  ((make-object de.michab.scream.fcos.SchemeInteger) TYPE_NAME))

#|
 | Encapsulates java.lang.Math.
 |#
(define scream:math
  (make-object java.lang.Math))

(define scream:class:number
  (make-object de.michab.scream.fcos.Number))

(define (scream:to-float x) (+ 0.0 x))

#|
 | Checks if the passed object if of type SchemeInteger.
 |#
(define scream:integer?
  (typePredicateGenerator "de.michab.scream.fcos.SchemeInteger" #t))
(define scream:double?
  (typePredicateGenerator "de.michab.scream.fcos.SchemeDouble" #t))

#|
 | Support-operation for the implementation of the math comparison operators.
 |
 | Expects an operation (op z1 z2) => bool.
 | Returns an operation (op' . z).
 |
 | Evalution is:
 |  (op' 1) => #t
 |  (op' 1 2 3) => (if (op 1 2)
 |                   (op' 2 3)
 |                   #f)
 |
 | The implementation uses quasiquote rewriting which is
 | performance-expensive.  Optimisation options are
 |  * Speed up quasiquote, this is preferred.
 |  * Delay evaluation.  See usages of case-lambda.
 |#
(define (scream:math:to-transitive-cmp cmp-operation)

  (define q-comparer
    `(lambda (z1 z2)
      (cond
        ((not (number? z1))
          (error "TYPE_ERROR" %type-number z1))
        ((not (number? z2))
          (error "TYPE_ERROR" %type-number z2))
        (else
          ((object z1) (,cmp-operation z2))))))

  (define comparer (scream:eval q-comparer))

  (define (transitiver first . rest)
    (cond
         ((not (number? first))
           (error "TYPE_ERROR" %type-number first))
         ((null? rest)
           #t)
         ((comparer first (car rest))
           (apply transitiver (car rest) (cdr rest)))
         (else
           #f)))

    transitiver)

#|
 | Support operation for implementing the min and max functions.
 |
 | compare is a comparison function (cmp a b) => boolean.
 |#
(define (scream:min-max compare inexact-seen first . rest)

    (if (null? rest)
      (if inexact-seen
        (inexact first)
        first)
      (let ((next (car rest)))
        (apply scream:min-max
          compare
          (or inexact-seen (inexact? first) (inexact? next))
          (if (compare first next)
            first
            next)
          (cdr rest)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; r7rs definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | (number? obj) procedure; r7rs 35
 |#
(define number?
  (typePredicateGenerator "de.michab.scream.fcos.Number" #f))

#|
 | (complex? obj) procedure; r7rs 35
 |#
(define (complex? obj)
  (error "NOT_IMPLEMENTED" 'complex?))

#|
 | (real? obj) procedure; r7rs 35
 |#
(define (real? obj)
  (number? obj))

#|
 | (complex? obj) procedure; r7rs 35
 |#
(define (rational? obj)
  (error "NOT_IMPLEMENTED" 'rational?))

#|
 | (integer? obj) procedure; r7rs 35
 |#
(define (integer? obj)
  (if (number? obj)
    (= (round obj) obj)
    #f))

#|
 | exact?
 |#
(define (exact? x)
  (if (number? x)
      ((object x) (isExact))
      (error "TYPE_ERROR"
             %type-number
             (scream:typename x))))

#|
 | inexact?
 |#
(define (inexact? x)
  (not (exact? x)))

#|
 | (exact-integer? z) library procedure; r7rs 38
 |#
(define (exact-integer? z) 
  (and
    (integer? z)
    (exact? z)))

#|
 | (finite? z) inexact library procedure; r7rs 35
 |#
(define (finite? obj)
  (error "NOT_IMPLEMENTED" 'finite?))

#|
 | (infinite? z) inexact library procedure; r7rs 35
 |#
(define (infinite? obj)
  (error "NOT_IMPLEMENTED" 'infinite?))

#|
 | (nan? z) inexact library procedure; r7rs 36
 |#
(define (nan? obj)
  (error "NOT_IMPLEMENTED" 'nan?))

#|
 | (= z₁ z₂ ...)  procedure; r7rs 36
 |#
(define =
  (scream:math:to-transitive-cmp 'r7rsEqual))

#|
 | (< z₁ z₂ ...)  procedure; r7rs 36
 |#
(define <
  (scream:math:to-transitive-cmp 'r7rsLessThan))

#|
 | (> z₁ z₂ ...)  procedure; r7rs 36
 |#
(define >
  (scream:math:to-transitive-cmp 'r7rsGreaterThan))

#|
 | (<= z₁ z₂ ...)  procedure; r7rs 36
 |#
(define <= 
  (scream:math:to-transitive-cmp 'r7rsLessOrEqualThan))
  
#|
 | (>= z₁ z₂ ...)  procedure; r7rs 36
 |#
(define >= 
  (scream:math:to-transitive-cmp 'r7rsGreaterOrEqualThan))

#|
 | (zero? z)
 |#
(define (zero? z)
  (= 0 z))

#|
 | positive? - library procedure - r7rs p36
 |#
(define (positive? x)
  (> x 0))

#|
 | negative? - library procedure - r7rs p36
 |#
(define (negative? x)
  (< x 0))

#|
 | (odd? n)
 |#
(define (odd? n)
  (if (integer? n)
    ((object (exact n)) (r7rsOddQ))
    (error "TYPE_ERROR"
             %type-integer
             (scream:typename n))))

#|
 | (even? n)
 |#
(define (even? n)
  (not (odd? n)))

#|
 | (max x₁ x₂ ...)  procedure; r7rs 36
 |#
(define (max n . rest)
  (apply scream:min-max > #f n rest))

#|
 | (min x₁ x₂ ...)  procedure; r7rs 36
 |#
(define (min n . rest)
  (apply scream:min-max < #f n rest))

;;
;; Set up dummy bindings in the global TLE.  The actual method definitions
;; will be done in the following let expression.
;;
(define abs ())

#|
 | (truncate x) procedure; r7rs 6.2.6 p37
 |#
(define (truncate x)
  (if (positive? x)
    (floor x)
    (ceiling x)))

(define exp ())
(define log ())
(define sin ())
(define cos ())
(define tan ())
(define asin ())
(define acos ())
(define atan ())

(define floor ())
(define ceiling ())

(define round ())

#|
 | (sqrt z) inexact library procedure; r7rs 38
 |#
(define (sqrt z) 
  (let ((result (scream:math (sqrt z))))
    (if (integer? result)
  	  (exact result)
      result
    )
  )
)

#|
 | (exact-integer-sqrt k) inexact library procedure; r7rs 38
 |#
(define (exact-integer-sqrt k) 
  (let*
    (
      (result (truncate (scream:math (sqrt k))))
      (result-square (* result result))
      (rest (- k result-square))
    )
    
    (values result rest)
  )
)

(define expt-float ())

;;
;; Open a special scope to keep the java.lang.Math instance in the local
;; closures.  Replace the TLE bindings with the function implementations in the
;; let expression.
;;
(let ((math (make-object java.lang.Math))
      (to-float (lambda (x) (+ 0.0 x)))
     )
  ;;
  ;; abs - library procedure - r5rs p. 22
  ;;
  (set! abs (lambda (x) (math (abs x))))

  ;;
  ;; exp - procedure - r5rs p. 23
  ;;
  (set! exp (lambda (x) (math (exp x))))

  ;;
  ;; log - procedure - r5rs p. 23
  ;;
  (set! log (lambda (x) (math (log x))))

  ;;
  ;; sin - procedure - r5rs p. 23
  ;;
  (set! sin (lambda (x) (math (sin x))))

  ;;
  ;; cos - procedure - r5rs p. 23
  ;;
  (set! cos (lambda (x) (math (cos x))))

  ;;
  ;; tan - procedure - r5rs p. 23
  ;;
  (set! tan (lambda (x) (math (tan x))))

  ;;
  ;; asin - procedure - r5rs p. 23
  ;;
  (set! asin (lambda (x) (math (asin x))))

  ;;
  ;; acos - procedure - r5rs p. 23
  ;;
  (set! acos (lambda (x) (math (acos x))))

  ;;
  ;; atan - procedure - r5rs p. 23
  ;;
  (set! atan
    (lambda x
      (if (= 1 (length x))
        (math (atan (car x)))
        (math (atan2 (car x) (cadr x))))))

#|
 | round - procedure - r7rs p37
 |#
  (set! round
    (lambda (x)
      (if (exact? x)
        x
        (scream:math (rint x)))))

  ;;
  ;; floor - procedure - r5rs p. 23
  ;;
  (set! floor 
    (lambda (x) 
      (round (math (floor x)))))

  ;;
  ;; ceiling - procedure - r5rs p. 23
  ;;
  (set! ceiling 
  	(lambda (x) 
  	  (round (math (ceil x)))))

  (set! expt-float (lambda (x y) (math (pow (to-float x) (to-float y)))))
)


(define (expt-int x y)
  (define (_expt-int x y)
    (if (eqv? y 1)
      x
      (* x (_expt-int x (- y 1)))))

  (cond 
    ((< y 0) (error "RANGE_EXCEEDED" y "y >= 0"))
    ((= y 0) 1)
    (else (_expt-int x y))))
    
#|
 | (expt z1 z2) procedure; r7rs 6.2.6 p38
 |#
(define (expt x y)
  (if (and (integer? x) (integer? y))
    (expt-int x y)
    (expt-float x y)))

;;
;; remainder - procedure - r5rs p. 22
;;
(define (remainder x y)
  ; Well that guy behaves magically.  Maps to the range of smallest abs
  ; values. Uh.
  (let ((result (round (scream:math (IEEEremainder (scream:to-float x)
                                                   (scream:to-float y))))))
    (cond ((and (positive? x) (negative? result))
           (+ result (abs y)))
          ((and (negative? x) (positive? result))
           (- result (abs y)))
          (else
           result))))



;;
;; (modulo n1 n2)
;;
(define (modulo x y)
  ; Well that guy behaves magically.  Maps to the range of smallest abs
  ; values. Uh.
  ; TODO micbinz scream:to-float should not be required.
  (let ((result (round (scream:math (IEEEremainder (scream:to-float x)
                                                   (scream:to-float y))))))
    (cond ((and (positive? y) (negative? result))
           (+ result (abs y)))
          ((and (negative? y) (positive? result))
           (- result (abs y)))
          (else
           result))))



;;
;; (gcd a b)
;;
(define (gcd . rest)
  (letrec ((positive-gcd
             (lambda (a b)
               (cond ((= a b)
                      a)
                     ((> a b)
                      (positive-gcd (- a b) b))
                     (else
                      (positive-gcd a (- b a)))))))

          (if (null? rest)
            0
            ; Take the argument list, run the abs procedure on each element and
            ; feed that into the locally defined procedure implementation.
            (apply positive-gcd (map abs rest)))))



;;
;;  static long lcm(Object args) {
;;    long L = 1, g = 1;
;;    while (isPair(args)) {
;;      long n = Math.abs((long)toInt(first(args)));
;;      g = gcd(n, L);
;;      L = (g == 0) ? g : (n / g) * L;
;;     args = toList(rest(args));
;;    }
;;    return L;
;;  }
(define (lcm . args)
  ;; TODO it must be possible to implement this simpler.
  (do ((k 1)
       (g 1)
       (n 0))
      ((null? args) k)
      (set! n (abs (car args)))
      (set! g (gcd n k))
      (set! k (if (= g 0)
                  g
                  (* k (/ n g))))
      (set! args (cdr args))))



;;
;; (quotient n1 n2) -- Integer division.
;;
(define (quotient n1 n2)
  (/ n1 n2))

#|
 | (inexact z) procedure; r7rs p39
 |#
(define (inexact z)
  (if (inexact? z)
    z
    (scream:to-float z)))

#|
 | (exact z) procedure; r7rs p39
 |#
(define (exact z)
  (if (number? z)
    ((object z) (r7rsExact))
    (error "TYPE_ERROR" scream:type-number z)))

; string->number supports a radix argument up to the number of entries
; in this vector.
(define %character-table
  #( #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F #\G))

(define (%number->string number char-list radix)
  (if (> radix (vector-length %character-table))
  	(error "RADIX_NOT_SUPPORTED" radix (vector-length %character-table)))
  (if (zero? number)
    ; If we received an empty character list...
    (if (null? char-list)
      ; ...we actually return a zero character.
      (cons (vector-ref %character-table 0) char-list)
      ; ...in the other case we return only the character
      ; list that we computed yet.
      char-list)
    (%number->string
      (quotient number radix)
      (cons
         (vector-ref %character-table (modulo number radix))
         char-list)
      radix)))

;;
;; TODO handle floating point
;;
(define (number->string number . opt-radix)
  (let* (
         (radix
           (cond
             ; If no optional radix was defined...
             ((null? opt-radix)
                ; ...we use the human 10 as default.
                10)
             ((= 1 (length opt-radix))
                (car opt-radix))
             (else
               (error "TOO_MANY_ARGUMENTS" 2))))

         (abs-character-list
           (%number->string (abs number) () radix))
       )
       (if (negative? number)
         (set! abs-character-list
         	(cons #\- abs-character-list)))
       (list->string abs-character-list)))

