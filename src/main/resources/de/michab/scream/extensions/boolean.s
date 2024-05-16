;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 1998-2024 Michael G. Binz
;

;;
;; (boolean? obj) library procedure; 6.3 r7rs 40
;;
(define boolean?
  scream:boolean?)

;;
;; (not obj) library procedure; r7rs 6.3 p40
;;
(define not
  scream:not)

;;
;; (boolean=? boolean1 boolean2 boolean3 ...) procedure r7rs 6.3 p 40
;;
(define (boolean=? first . rest)

  (define (boolean-impl position result list)
    (if (null? list)
      #t
      (let
        (
          (first (scream:assert:boolean 'boolean=? (car list)))
          (rest (cdr list))
        )
        (cond
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
