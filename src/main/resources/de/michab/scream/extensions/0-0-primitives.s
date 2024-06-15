;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 2024 Michael G. Binz
;

#|
(define (scream:eq? x y)
  (scream:class:fco
    ("eq:de.michab.scream.fcos.FirstClassObject,de.michab.scream.fcos.FirstClassObject"
    x
    y))
)

(define (scream:eqv? x y)
  (scream:class:fco
    ("eqv:de.michab.scream.fcos.FirstClassObject,de.michab.scream.fcos.FirstClassObject"
    x
    y))
)

(define (scream:equal? x y)
  (scream:class:fco 
    ("equal:de.michab.scream.fcos.FirstClassObject,de.michab.scream.fcos.FirstClassObject"
    x
    y))
)
|#

(define (scream:not obj)
  (scream:eqv? obj #f))

; scream:null? --defined as Java primitive.

; scream:car - defined as Java primitive;

; scream:cdr - defined as Java primitive;

(define (scream:positive? number)
  ((object number) ("r7rsGreaterOrEqualThan:de.michab.scream.fcos.Number" 0))
)

(define (scream:string-append s1 s2)
  ((object s1) ("append:de.michab.scream.fcos.SchemeString" s2))
)

