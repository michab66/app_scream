;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 2024 Michael G. Binz
;

;;
;; r7rs 6.4 Pairs and lists, p40
;;

#|
 | (make-list k) procedure; r7rs 6.4 p42
 | (make-list k fill) procedure; r7rs 6.4 p42
 |#
(define make-list

  (scream:delay-op (delay ; -->

  (case-lambda

    ((k)
      (make-list k scream:unspecified))

    ((k fill)
      (cond
        ((not (integer? k))
          (error "TYPE_ERROR" scream:type-integer k))
        ((<= k 0)
          '())
        (else
          (cons fill (make-list (- k 1) fill)))))

  ) ; case-lambda

  )) ; <--
)

#|
 | (member obj list) procedure; r7rs 6.4 p43
 | (member obj list compare) procedure; r7rs 6.4 p43
 |#
(define member

  (scream:delay-op (delay ; -->

  (case-lambda

    ((obj list)
      (member obj list equal?))

    ((obj list compare)
      (scream:memx obj list compare))

  ) ; case-lambda

  )) ; <--
)

#|
 | (assoc obj alist) procedure; r7rs 6.4 p43
 | (assoc obj alist compare) procedure; r7rs 6.4 p43
 |#
(define assoc

  (scream:delay-op (delay ; -->

  (case-lambda

    ((obj list)
      (assoc obj list equal?))

    ((obj list compare)
      (scream:assx obj list compare))

  ) ; case-lambda

  )) ; <--
)
