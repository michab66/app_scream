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

;;
(define scream:class:fco
  (make-object "de.michab.scream.fcos.FirstClassObject"))

;;
;; An object to be returned if the spec defines 'unspecified'.
;;
(define scream:unspecified '())

; Perform common runtime initialization.
(include
  "0-0-primitives.s"
  "0-1-runtime-tools.s"
  "0-2-type-asserts.s"

  "6_1_equivalence-predicates.s"
  "object.s"
  "boolean.s"
  "vector.s"
  "delayed.s"
  "6_4_Pairs_and_lists.s"
  "string.s"
  "6_10_Control_features.s"
  "quasiquote.s"
  "6_2_Numerical_operations.s"
  "case-lambda.s"
  "symbol.s"
  "char.s"
  "bytevector.s"
  "cons-delayed.s"
  "6_14_System_interface.s"

  "i-ports.s"
  "i-environment.s"
)
