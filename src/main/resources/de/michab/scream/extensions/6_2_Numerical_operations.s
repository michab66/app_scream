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
  ((make-object de.michab.scream.fcos.Int) TYPE_NAME))

#|
 | Encapsulates java.lang.Math.
 |#
(define scream:math
  (make-object java.lang.Math))

(define scream:class:number
  (make-object de.michab.scream.fcos.Number))

(define (scream:to-float x) (+ 0.0 x))

#|
 | Checks if the passed object if of type Int.
 |#
(define scream:integer?
  (typePredicateGenerator "de.michab.scream.fcos.Int" #t))
(define scream:double?
  (typePredicateGenerator "de.michab.scream.fcos.Real" #t))

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
  (scream:delay-op (delay ; -->
    (scream:math:to-transitive-cmp 'r7rsEqual)
  )) ; <--
)

#|
 | (< z₁ z₂ ...)  procedure; r7rs 36
 |#
(define <
  (scream:delay-op (delay ; -->
    (scream:math:to-transitive-cmp 'r7rsLessThan)
  )) ; <--
)

#|
 | (> z₁ z₂ ...)  procedure; r7rs 36
 |#
(define >
  (scream:delay-op (delay ; -->
    (scream:math:to-transitive-cmp 'r7rsGreaterThan)
  )) ; <--
)

#|
 | (<= z₁ z₂ ...)  procedure; r7rs 36
 |#
(define <= 
  (scream:delay-op (delay ; -->
    (scream:math:to-transitive-cmp 'r7rsLessOrEqualThan)
  )) ; <--
)
  
#|
 | (>= z₁ z₂ ...)  procedure; r7rs 36
 |#
(define >=
  (scream:delay-op (delay ; -->
    (scream:math:to-transitive-cmp 'r7rsGreaterOrEqualThan)
  )) ; <--
)

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
 | (odd? n) procedure; r7rs p36
 |#
(define (odd? n)
  (if (integer? n)
    ((object (exact n)) (r7rsOddQ))
    (error "TYPE_ERROR"
             %type-integer
             (scream:typename n))))

#|
 | (even? n) procedure; r7rs p36
 |#
(define (even? n)
  (not (odd? n)))

#|
 | (max x₁ x₂ ...)  procedure; r7rs p36
 |#
(define (max n . rest)
  (apply scream:min-max > #f n rest))

#|
 | (min x₁ x₂ ...)  procedure; r7rs p36
 |#
(define (min n . rest)
  (apply scream:min-max < #f n rest))

#|
 | (abs x) procedure; r7rs p36
 |#
(define (abs x)
  (if (number? x)
    (scream:math (abs x))
    (error "TYPE_ERROR"
             %type-integer
             (scream:typename x))))

#|
 | (floor/ n₁ n₂) procedure r7rs p36
 |#
(define (floor/ n1 n2)
  (values
    (floor-quotient n1 n2)
    (floor-remainder n1 n2)))

#|
 | (floor-quotient n₁ n₂) procedure r7rs p36
 |#
(define (floor-quotient n1 n2)
  (scream:math (floorDiv n1 n2)))
  
(define (floor-quotient n1 n2)
  (if (not (integer? n1))
    (error "TYPE_ERROR" %type-integer n1))
  (if (not (integer? n2))
    (error "TYPE_ERROR" %type-integer n2))
    
  ((if (and (exact? n1) (exact? n2))
      exact
      inexact)
  (scream:math (floorDiv (exact n1) (exact n2)))))

#|
 | (floor-remainder n₁ n₂) procedure r7rs p36
 |#
(define (floor-remainder n1 n2)
  (let ((nq (floor-quotient n1 n2)))
    (- n1 (* n2 nq))))

#|
 | (truncate/ n₁ n₂) procedure r7rs p36
 |#
(define (truncate/ n1 n2)
  (values
    (truncate-quotient n1 n2)
    (truncate-remainder n1 n2)))

#|
 | (truncate-quotient n₁ n₂) procedure r7rs p36
 |#
(define (truncate-quotient n1 n2)
  ((if (and (exact? n1) (exact? n2))
      exact
      inexact)
  (truncate (/ n1 n2))))

#|
 | (truncate-remainder n₁ n₂) procedure r7rs p36
 |#
(define (truncate-remainder n1 n2)
  (let ((nq (truncate-quotient n1 n2)))
    (- n1 (* n2 nq))))

#|
 | (quotient n₁ n₂) procedure; r7rs 6.2.6 p37
 |#
(define quotient truncate-quotient)

#|
 | (remainder n₁ n₂) procedure; r7rs 6.2.6 p37
 |#
(define remainder truncate-remainder)

#|
 | (modulo n₁ n₂) procedure; r7rs 6.2.6 p37
 |#
(define modulo floor-remainder)

#|
 | (gcd n1 ...) procedure 6.2.6 p37
 |#
(define gcd
  (letrec 
    (
      (_gcd 
        (lambda (a b)
          (abs
            (if (zero? b)
              a
              (_gcd b (remainder a b))))))
            
      (_gcd-transitive
        (scream:to-transitive _gcd))
    )

    (lambda arguments
      (cond
        ((null? arguments)
          0)
        ((= 1 (length arguments))
          (if (number? (car arguments))
            (car arguments)
            (error "TYPE_ERROR" %type-integer n1)))
        (else
          (apply _gcd-transitive arguments))))
  )
)

#|
 | (lcm n1 ...) procedure 6.2.6 p37
 |#
(define lcm
  (letrec 
    (
      (_lcm 
        (lambda (a b)
          (if (or (zero? a) (zero? b))
            0
            (abs (* b (floor (/ a (gcd a b))))))))

      (_lcm-exact
        (lambda (a b)
          (let
            (
              (inexact-seen (or (inexact? a) (inexact? b)))
              (result (_lcm a b))
            )

            (if inexact-seen
              result
              (exact result)))))

      (_lcm-transitive
        (scream:to-transitive _lcm-exact))
    )

    (lambda arguments
      (cond
        ((null? arguments)
          1)
        ((= 1 (length arguments))
          (if (number? (car arguments))
            (car arguments)
            (error "TYPE_ERROR" %type-integer n1)))
        (else
          (apply _lcm-transitive arguments))))
  )
)

#|
 | (numerator q)
 |#
(define (numerator q)
  (error "NOT_IMPLEMENTED" 'numerator))

#|
 | (denominator q)
 |#
(define (denominator q)
  (error "NOT_IMPLEMENTED" 'denominator))

#|
 | (floor x) procedure 6.2.6 p37
 |#
(define (floor x)
  (cond
    ((not (number? x))
      (error "TYPE_ERROR" %type-number x))
    ((integer? x)
      x)
    (else
      (round (scream:math (floor x))))))

#|
 | (ceiling x) procedure - 6.2.6 p37
 |#
(define (ceiling x)
  (cond
    ((not (number? x))
      (error "TYPE_ERROR" %type-number x))
    ((integer? x)
      x)
    (else
      (round (scream:math (ceil x))))))

#|
 | (truncate x) procedure; r7rs 6.2.6 p37
 |#
(define (truncate x)
  (cond
    ((exact? x)
      x)
    ((positive? x)
      (floor x))
    (else
      (ceiling x))))

(define exp ())
(define log ())
(define sin ())
(define cos ())
(define tan ())
(define asin ())
(define acos ())
(define atan ())

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

  (if (not (and (integer? k) (>= k 0) (exact? k)))
    (error "TYPE_ERROR" "nonnegative exact integer" k))

  (let*
    (
      (result
        (exact (truncate (scream:math (sqrt k)))))
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
