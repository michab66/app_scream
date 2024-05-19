;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 2024 Michael G. Binz
;

#|
 | eq? obj₁ obj₂ ...)) r7rs 6.1 p30 procedure
 |#
(define eq?
  scream:eq?
)

#|
 | (eqv? obj₁ obj2₂ ...)) r7rs 6.1 p31 procedure
 |#
(define eqv?
  scream:eqv?
)

#|
 | (equal obj₁ obj₂ ...)) r7rs 6.1 p32 procedure
 |#
(define equal?
  scream:equal?
)
