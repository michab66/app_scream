; Scream @ https://github.com/urschleim/scream
;
; Copyright © 2023 Michael G. Binz

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; r7rs definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(%syntax (delay expression)
  (vector 'promise #f expression))

; delay-force

(define (promise? obj)
  (and
    (vector? obj)
    (eqv? 3 (vector-length obj))
    (eqv? 'promise (vector-ref obj 0)))
)

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

; make-promise
(define (make-promise obj)
  (if (promise? obj)
    obj
    (vector 'promise obj '())))

(define (current-jiffy)
  ((make-object java.lang.System) (currentTimeMillis)))

(define (jiffies-per-second)
  1000)
