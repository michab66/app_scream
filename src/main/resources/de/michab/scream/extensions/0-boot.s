;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 1998-2024 Michael G. Binz
;

(define
  scream:evaluator
  ((make-object "de.michab.scream.ScreamEvaluator") ("EVAL"))
)

(define
  scream:java:lang:system
  (make-object "java.lang.System")
)
(define
  scream:java:lang:math
  (make-object "java.lang.Math")
)

;;
(define scream:class:fco
  (make-object "de.michab.scream.fcos.FirstClassObject"))
(define scream:class:number
  (scream:java:make-class "de.michab.scream.fcos.Number"))

;;
;; An object to be returned if the spec defines 'unspecified'.
;;
(define scream:unspecified '())

; Perform common runtime initialization.
(include
  "0-0-primitives.s"
  "0-1-runtime-tools.s"
  "0-2-type-asserts.s"

  "6-1-equivalence-predicates.s"
  "object.s"
  "6-3-boolean.s"
  "6-8-vector.s"
  "4-2-5-delayed.s"
  "6-4-pairs_and_lists.s"
  "6-6-char.s"
  "6-7-string.s"
  "6-10-control_features.s"
  "4-2-8-quasiquote.s"
  "6_2_Numerical_operations.s"
  "4-2-9-case-lambda.s"
  "6-5-symbol.s"
  "6-9-bytevector.s"
  "cons-delayed.s"
  "6_14_System_interface.s"

  "6-13-ports.s"
  "6-13-2-input.s"
  "6-13-3-output.s"
  
  "6-12-environment.s"
)
