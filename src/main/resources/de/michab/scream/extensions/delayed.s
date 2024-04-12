;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 2023 Michael G. Binz
;

(define (scream:delay-op promise)
  (lambda args
    (apply (force promise) args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; r7rs definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | (delay expression)  lazy library procedure, r7rs p18
 |#
(%syntax (delay expression)
  (vector 'promise #f expression))

; delay-force

#|
 | (promise? obj)  lazy library procedure, r7rs p19
 |#
(define (promise? obj)
  (and
    (vector? obj)
    (eqv? 3 (vector-length obj))
    (eqv? 'promise (vector-ref obj 0)))
)

#|
 | (force promise)  lazy library procedure, r7rs p18
 |#
(define (force promise)
  (cond
    ((vector-ref promise 1)
      (vector-ref promise 1))
    (else
      (vector-set!
        promise
        1
        (scream:eval (vector-ref promise 2)))
      (vector-ref promise 1)))
)

#|
 | (make-promise obj)  lazy library procedure, r7rs p19
 |#
(define (make-promise obj)
  (if (promise? obj)
    obj
    (vector 'promise obj '())))
