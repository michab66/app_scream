
;      -*- Mode: Lisp -*-                             Filename:  pmath.s

;                     Last Revision:  12-Sep-85 1930ct

;--------------------------------------------------------------------------;
;                                                                          ;
;                         TI SCHEME -- PCS Compiler                        ;
;                  Copyright 1985 (c) Texas Instruments                    ;
;                                                                          ;
;                      Clyde R. Camp, David Bartley,                       ;
;                        Mark Meyer, John Gateley                          ;
;                                                                          ;
;                      Extended Arithmetic Routines                        ;
;                                                                          ;
;--------------------------------------------------------------------------;


(define exact?                                          ; EXACT?
  (lambda (n)
    #!false))

(define inexact?                                        ; INEXACT?
  (lambda (n)
    #!true))

(begin
  (define acos)
  (define asin)
  (define atan)
  (define cos)
  (define exp)
  (define expt)
  (define log)
  (define sin)
  (define sqrt)
  (define tan)
  (define pi)
  )

(letrec
 ((   *pi*      3.141592653589793)              ; pi
  (   *pi/2*    (/ *pi* 2))                     ; pi/2
  (   *2pi*     (+ *pi* *pi*))                  ; 2pi
  (   *e*       2.718281828459045)              ; e

  (%bad-argument
   (lambda (name arg)
     (%error-invalid-operand name arg)))

  (signum
   (lambda (x)
     (cond ((negative? x) -1)
           ((positive? x) 1)
           (else 0))))

  (power-loop
   (lambda (x n a)      ; A is initially 1, N is non-negative
     (if (zero? n)
         a
         (power-loop (* x x)
                     (quotient n 2)
                     (if (odd? n) (* a x) a)))))

  (pcs-series
    (lambda (x y z)
      (if (null? y)
          z
          (pcs-series x (cdr y) (- 1.0 (* (/ x (car y)) z))))))

  (fact-series
   (lambda (x n result)
     (if (zero? n)
         result
         (fact-series x (- n 1) (+ 1 (* (/ x n) result))))))
  )
 (begin

   (set! sqrt
         (letrec ((loop (lambda (x gx)
                          (let ((ngx (* 0.5 (+ gx (/ x gx)))))
                            (if (>? (/ (abs (- ngx gx)) gx) 5e-15)
                                (loop x ngx)
                                ngx)))))
           (named-lambda (sqrt x)
             (if (or (not (number? x)) (negative? x))
                 (%bad-argument 'SQRT x)
                 (let ((x (float x)))
                   (if (zero? x)
                       x
                       (cond ((>? x 1.0e10)(* 1.0e5 (sqrt (* x 1.0e-10))))
                             ((<? x 1.0e-10)(* 1.0e-5 (sqrt (* x 1.0e10))))
                             (else (loop x 1.0)))))))))

   (set! sin
         (lambda (x)
           (if (not (number? x))
               (%bad-argument 'SIN x)
               (begin
                 (set! x (/ x *2pi*))
                 (set! x (* *2pi* (- x (truncate x))))
                 (when (>? x *pi*)
                       (set! x (- x *2pi*)))
                 (if (>? x *pi/2*)
                     (set! x (- *pi* x))
                     (when (<? x (- *pi/2*))
                           (set! x (- (+ *pi* x)))))
                 ; Now X lies in the interval [-pi/2, pi/2]
                 (let ((term x)
                       (x2 (* x x))
                       (lim (ceiling (+ 12 (abs (* x 8))))))
                   (let ((ssum (do ((sum x (+ sum term))
                                    (n 2 (+ n 2)))
                                   ((>=? n lim) (+ sum term))
                                 (set! term (- (/ (* term x2)
                                                  (* n (+ n 1))))))  ))
                     ; The following limits (sin x) to +/- 1
                     ; without it result can be 1.0 + 1e-18
                     ; which blows up ASIN
                     (cond ((>? ssum 1.0) 1.0)
                           ((<? ssum -1.0) -1.0)
                           (else ssum)) ) )))))

   (set! cos
         (lambda (x)
           (if (not (number? x))
               (%bad-argument 'COS x)
               (sin (+ x *pi/2*)))))

   (set! tan
         (lambda (x)
           (if (not (number? x))
               (%bad-argument 'TAN x)
               (let ((y (sin x))
                     (z (cos x)))
                 (if (zero? z)
                     (%bad-argument 'TAN x)
                     (/ y z))))))

   (set! atan
         (named-lambda (atan y . z)
           (if (not (number? y))
               (%bad-argument 'ATAN y)
               (letrec ((loop (lambda (y k)
                                (if (=? k 10)
                                    0.0
                                    (/ (* y k k)
                                       (+ 1 k k (loop y (+ k 1)) ))))) )
                       (if (null? z)
                           (cond ((negative? y)
                                  (minus (atan (minus y))))
                                 ((>? y 1.0)
                                  (- *pi/2* (atan (/ 1.0 y))))
                                 (else
                                  (/ y (+ 1 (loop (* y y) 1)))))
                           (let ((x (car z)))
                             (cond ((not (number? x))
                                    (%bad-argument 'ATAN x))
                                   ((zero? x)
                                    (cond ((zero? y)
                                           (%bad-argument 'ATAN
                                                          x))
                                          ((negative? y)
                                           (minus *pi/2*))
                                          (else *pi/2*)))
                                   ((zero? y)
                                    (if (positive? x)  0.0 *pi*))
                                   ((positive? y)
                                    (if (>? x 0)
                                        (atan (/ y x))
                                        (- *pi/2* (atan (/ x y)))))
                                   ((and (<? x 0)
                                         (<? y 0))
                                    (minus (- *pi* (atan (/ y x)))))
                                   (else
                                    (minus (+ *pi/2* (atan (/ x y)))))) )
                           )))))

   (set! acos
         (lambda (x)
           (if (or (not (number? x))
                   (>? (abs x) 1.0))
               (%bad-argument 'ACOS x)
               (atan (sqrt (- 1.0 (* x x))) x))))

   (set! pi *pi*)

   (set! asin
         (lambda (x)
           (if (or (not (number? x))
                   (>? (abs x) 1.0))
               (%bad-argument 'ASIN x)
               (atan x (sqrt (- 1.0 (* x x)))))))

   (set! log
         (named-lambda (log x . base)
           (letrec
             ((ln (lambda (x)
                    (cond ((=? x 1) 0)
                          ((<? x 1.0) (minus (ln (/ x))))
                          ((>? x *e*) (1+ (ln (/ x *e*))))
                          (else (let ((y (/ (-1+ x) (1+ x))))
                                  (* (pcs-series (* y y)
                                                 '(-1.0952380952381
                                                   -1.10526315789474
                                                   -1.11764705882353
                                                   -1.33333333333333
                                                   -1.15384615384615
                                                   -1.18181818181818
                                                   -1.22222222222222
                                                   -1.28571428571429
                                                   -1.4
                                                   -1.66666666666667
                                                   -3.0)
                                                 1.0)
                                     (+ y y)))) ))))
             (if (or (not (number? x)) (<=?  x 0))
                 (%bad-argument 'LOG x)
                 (let ((lnx (ln x)))
                   (if (null? base)
                       lnx
                       (let ((non-e-base (car base)))
                         (if (or (not (number? non-e-base))
                                 (not (positive? non-e-base)))
                             (%bad-argument 'LOG non-e-base)
                             (/ lnx (log non-e-base))))))))))

   (set! exp
         (named-lambda (exp x)
           (cond ((not (number? x))
                  (%bad-argument 'EXP x))
                 ((zero? x) 1.0)
                 ((negative? x) (/ (exp (- x))))
                 ((integer? x) (power-loop *e* x 1))
                 (else
                  (let* ((q (truncate x))
                         (p (- x q)))
                    (* (power-loop *e* q 1)
                       (fact-series p 12 1)))))))

   (set! expt
         (named-lambda (expt a x)
           (cond ((not (number? a))
                  (%bad-argument 'EXPT a))
                 ((not (number? x))
                  (%bad-argument 'EXPT x))
                 ((and (zero? a) (zero? x) (not (integer? x)))
                  (%bad-argument 'EXPT x))
                 ((zero? x)  (if (integer? a) 1 1.0))
                 ((negative? x) (/ (expt a (minus x))))
                 ((integer? x) (power-loop a x 1))
                 (else
                  (let* ((z  (* x (log (abs a))))
                         (q (truncate z))
                         (p (- z q)))
                    (* (if (negative? q)
                           (/ (power-loop *e* (minus q) 1))
                           (power-loop *e* q 1))
                       (signum a)
                       (fact-series p 12 1)))))  ))
   ))
