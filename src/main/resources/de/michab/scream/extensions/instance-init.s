;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 1998-2023 Michael G. Binz
;

;;
;; (load string), library procedure, r7rs 59
;;
;; Note that this overrides the (load ...)-procedure defined in
;; Scream.java.  The override loads always into the interaction
;; environment.
;; TODO fix to support the parameters described in the spec.
;;
;(define (load string)
;  (scream:evaluator (load string)))

(include "i-ports.s")
(include "i-environment.s")
