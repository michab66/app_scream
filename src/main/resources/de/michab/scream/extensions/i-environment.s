;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 2023 Michael G. Binz
;

;;
;; r7rs 6.12 p55 
;;

; TODO NOT TESTED
(define (scheme-report-environment v)
  (if (not (integer? v))
    (error
      "TYPE_ERROR"
      (make-object "de.michab.scream.fcos.Int")
      ((object v) ("getClass"))))
  (if (or (< v 5) (> v 7))
    (error "ILLEGAL_ARGUMENT" v))
  scream:tle-interpreter)

; TODO NOT TESTED
(define (null-environment v)
  (if (not (integer? v))
    (error
      "TYPE_ERROR" 
      (make-object "de.michab.scream.fcos.Int")
      ((object v) ("getClass"))))
  (if (or (< v 5) (> v 7))
    (error "ILLEGAL_ARGUMENT" v))
  scream:null-environment)

(define (interaction-environment)
  (scream:evaluator (getInteraction)))
