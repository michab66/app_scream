; Scream @ https://github.com/urschleim/scream
;
; Copyright © 1998-2022 Michael G. Binz

;;
;; Used in error messages.
;;
(define scream:type-bool
  ((make-object de.michab.scream.fcos.SchemeBoolean) TYPE_NAME))

;;
;; (boolean? obj) library procedure; 6.3 r7rs 40
;;
(define (boolean? obj)
  (or (eqv? obj #t) (eqv? obj #f)))

;;
;; (not obj) library procedure; r7rs 6.3 p40
;;
(define (not obj)
  (eqv? obj #f))

;;
;; (boolean=? boolean1 boolean2 boolean3 ...) procedure r7rs 6.3 p 40
;;
(define (boolean=? first . rest)

  (define (boolean-impl position result list)
    (if (null? list)
      #t
      (let ((first (car list)) (rest (cdr list)))
        (cond
          ((not (boolean? first))
            (error "TYPE_ERROR" scream:type-bool (%typename first) position))
          ((eq? result first)
            (boolean-impl (+ position 1) result rest))
          (else
            #f)
        ) ; cond
      ) ; let
    ) ; if
  ) ; define
  
  ; Add 'first to the passed list, so that
  ; type checking is performed in 'boolean-impl.
  (boolean-impl 1 first (cons first rest))
)
