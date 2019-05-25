
;      -*- Mode: Lisp -*-                             Filename:  pmath.s

;--------------------------------------------------------------------------;
;                                                                          ;
;                         TI SCHEME -- PCS Compiler                        ;
;                  Copyright 1987 (c) Texas Instruments                    ;
;                            All Rights Reserved                           ;
;                                                                          ;
;  Extended Arithmetic Routines using XLI/Lattice C 8087/80287 NDP support ;
;                                                                          ;
;                              Bob Beal                                    ;
;                                                                          ;
;--------------------------------------------------------------------------;


(define exact? (lambda (n) #f))

(define inexact? (lambda (n) #t))

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
 (
;  (   *pi*      3.141592653589793)              ; pi
;  (   *pi/2*    (/ *pi* 2))                     ; pi/2
;  (   *2pi*     (+ *pi* *pi*))                  ; 2pi
  (   *e*       2.718281828459045)              ; e

  (%bad-argument
   (lambda (name arg)
     (%error-invalid-operand name arg)))

  (power-loop
   (lambda (x n a)      ; A is initially 1, N is non-negative
     (if (zero? n)
         a
         (power-loop (* x x)
                     (quotient n 2)
                     (if (odd? n) (* a x) a)))))
  )
 (begin

   (set! sqrt
         (lambda (x)
           (if (or (not (number? x)) (negative? x))
               (%bad-argument 'sqrt x)
               (let ((x (float x)))
                 (if (zero? x)
                     x
                     (xcall "sqrt" (float x)))))))
   (set! sin
         (lambda (x)
           (if (not (number? x))
               (%bad-argument 'sin x)
               (xcall "sin" (float x)))))

   (set! cos
         (lambda (x)
           (if (not (number? x))
               (%bad-argument 'cos x)
               (xcall "cos" (float x)))))


   (set! tan
         (lambda (x)
           (if (not (number? x))
               (%bad-argument 'tan x)
               (xcall "tan" (float x)))))

   (set! atan
         (lambda (x . z)
           (cond ((not (number? x))
                  (%bad-argument 'atan x))
                 ((null? z)
                  (xcall "atan" (float x)))
                 ((not (number? (car z)))
                  (%bad-argument 'atan z))
                 (else
                  (xcall "atan2" (float x) (float (car z)))))))

   (set! acos
         (lambda (x)
           (if (or (not (number? x))
                   (>? (abs x) 1.0))
               (%bad-argument 'ACOS x)
               (xcall "acos" (float x)))))

   (set! pi (acos -1))  ;it'd be easier to set pi to a constant but make_fsl
                        ;is not quite up to 8087 long-real precision on
                        ;literal constants (e.g. (tan (/ pi 4)) is +/- 2
                        ;in the last digit via make_fsl, but +/- 0 if typed
                        ;in at toplevel or computed as here)

   (set! asin
         (lambda (x)
           (if (or (not (number? x))
                   (>? (abs x) 1.0))
               (%bad-argument 'ASIN x)
               (xcall "asin" (float x)))))

   (set! log
         (lambda (x . base)
           (cond ((or (not (number? x)) (<= x 0))
                  (%bad-argument 'log x))
                 ((null? base)
                  (xcall "ln" (float x)))
                 ((eq? (car base) 10)             ;the eq? is deliberate
                  (xcall "log10" (float x)))
                 ((= (car base) 1.0)
                  (error "Divide by zero" 'log x (car base)))
                 (else
                  (let ((non-e-base (car base)))
                    (if (or (not (number? non-e-base))
                            (not (positive? non-e-base)))
                        (%bad-argument 'log non-e-base)
                        (xcall "log" (float x) (float non-e-base))))))))

   (set! exp
         (lambda (x)
           (cond ((not (number? x))
                  (%bad-argument 'EXP x))
                 ((zero? x) 1.0)
                 ((negative? x) (/ (xcall "exp" (- (float x)))))
                 ((integer? x) (power-loop *e* x 1))
                 (else
                  (xcall "exp" (float x))))))

   (set! expt
         (lambda (a x)
           (cond ((not (number? a))
                  (%bad-argument 'EXPT a))
                 ((not (number? x))
                  (%bad-argument 'EXPT x))
                 ((and (zero? a) (zero? x) (not (integer? x)))
                  (%bad-argument 'EXPT x))
                 ((zero? x)  (if (integer? a) 1 1.0))
                 ((negative? x) (/ (xcall "expt" (float a) (- (float x)))))
                 ((integer? x) (power-loop a x 1))
                 (else
                  (xcall "expt" (float a) (float x))))))
   ))
