; Scream @ https://github.com/urschleim/scream
;
; Copyright © 2024 Michael G. Binz

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scream definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; r7rs definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | (current-jiffy)  time library procedure; r7rs p60
 |#
(define (current-jiffy)
  ((make-object java.lang.System) (currentTimeMillis)))

#|
 | (jiffies-per-second)  time library procedure; r7rs p60
 |#
(define (jiffies-per-second)
  1000)

#|
 | (current-second)  time library procedure; r7rs p60
 |#
(define (current-second)
  (/ (current-jiffy) (jiffies-per-second)))
