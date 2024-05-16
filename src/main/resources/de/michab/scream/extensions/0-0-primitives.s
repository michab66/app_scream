;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 2024 Michael G. Binz
;

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

(define (scream:not obj)
  (scream:eqv? obj #f))

(define (scream:null? obj)
  (scream:eqv? '() obj))

(define (scream:car pair)
  ((object pair) ("getCar"))
)
(define (scream:cdr pair)
  ((object pair) ("getCdr"))
)
