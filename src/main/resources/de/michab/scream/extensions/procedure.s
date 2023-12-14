; Scream @ https://github.com/urschleim/scream
;
; Copyright © 1998-2023 Michael G. Binz

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scream definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Procedure type name.
(define scream:type-procedure
  ((make-object de.michab.scream.fcos.Procedure) TYPE_NAME))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; r7rs definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | (procedure? obj) essential procedure; r5rs 27
 |#
(define procedure?
  (typePredicateGenerator "de.michab.scream.fcos.Procedure" #f))

#|
 | (apply proc arg1 ... args)  procedure; r7rs 50
 |#
(define (apply op . list)
  (define (make-argument-list list)
    (let ((first (car list)) (rest (cdr list)))
      (if (null? rest)
        first
        (cons first (make-argument-list rest)))))
  (scream:apply op (make-argument-list list)))

#|
 | (map proc list1 list2 ... ) r7rs 6.10 p51 procedure
 |#
(define (map procedure . lists)
  ;(display "_map:lists= ") (display lists)(newline)

  (define (p-all-circular lists)
    (cond
      ((null? lists) #t)
      ((not (circular? (car lists))) #f)
      (else (p-all-circular (cdr lists)))))

  (define (p-simple-car lists)
    (if (null? lists)
      '()
      (cons (caar lists) (p-simple-car (cdr lists)))))
  (define (p-simple-cdr lists)
    (if (null? lists)
      '()
      (cons (cdar lists) (p-simple-cdr (cdr lists)))))

  (define (__map procedure lists)
;   (display "__map: ") (display lists) (newline)
    (cond
      ((null? lists)
        '())
      ((memv '() lists)
        '())
      (else
        (let
          (
            (slice (p-simple-car lists))
            (roast (p-simple-cdr lists))
          )
;          (display "slice: ") (display slice) (newline)
          (cons (apply procedure slice) (__map procedure roast))))
    )
  )

  (cond
    ((not (procedure? procedure))
      (error "TYPE_ERROR" scream:type-procedure procedure))
    ((p-all-circular lists)
      (error "ILLEGAL_ARGUMENT" "Only circular lists"))
    (else
      (__map procedure lists))))

;;
;; (for-each proc list1 list2 ... ) library procedure
;;
(define for-each map)

#|
 | (values obj ...) r7rs 6.10 p53 procedure
 |#
(define (values . things)
     (call-with-current-continuation
        (lambda (cont) (apply cont things))))

#|
 | (call-with-values producer consumer) r7rs 6.10 p53 procedure
 |#
;; Implemented in Continuation.java

;;
;; See https://github.com/urschleim/scream/issues/221
;;
(define (eq? x y)
    (%fco-class (eq x y)))

(define (eqv? x y)
    (%fco-class (eqv x y)))

(define (equal? x y)
    (%fco-class (equal x y)))
