;
; Scream @ https://github.com/michab/dev_smack
;
; Copyright © 1998-2023 Michael G. Binz
;

;;
;; Access the environment user expressions are evaluated in.
;;
(define (interaction-environment)
  (scream::evaluator (getTopLevelEnvironment)))

;;
;; (load string), library procedure, r7rs 59
;;
;; Note that this overrides the (load ...)-procedure defined in
;; Scream.java.  The override loads always into the interaction
;; environment.
;; TODO fix to support the parameters described in the spec.
;;
(define (load string)
  (scream::evaluator (load string)))

(load "i-ports.s")
