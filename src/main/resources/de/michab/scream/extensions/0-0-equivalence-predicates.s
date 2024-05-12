;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 2024 Michael G. Binz
;

#|
 | eq? obj₁ obj₂ ...)) r7rs 6.1 p30 procedure
 |#
(define (eq? x y)
  (scream:class:fco
    ("eq:de.michab.scream.fcos.FirstClassObject,de.michab.scream.fcos.FirstClassObject"
    x
    y))
)

#|
 | (eqv? obj₁ obj2₂ ...)) r7rs 6.1 p31 procedure
 |#
(define (eqv? x y)
  (scream:class:fco
    ("eqv:de.michab.scream.fcos.FirstClassObject,de.michab.scream.fcos.FirstClassObject"
    x
    y))
)

#|
 | (equal obj₁ obj₂ ...)) r7rs 6.1 p32 procedure
 |#
(define (equal? x y)
  (scream:class:fco 
    ("equal:de.michab.scream.fcos.FirstClassObject,de.michab.scream.fcos.FirstClassObject"
    x
    y))
)
