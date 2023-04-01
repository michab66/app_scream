; $Id: procedure.s 8 2008-09-14 14:23:20Z binzm $
;
; Scream / Runtime
;
; Released under Gnu Public License
; Copyright (c) 1998-2000 Michael G. Binz

;;
;; Scream definitions.
;;

;; Procedure type name.
(define scream:type-procedure
  ((make-object de.michab.scream.fcos.Procedure) TYPE_NAME))


;;
;; r7rs
;;

;;
;; (procedure? obj) essential procedure; r5rs 27
;;
(define procedure?
  (typePredicateGenerator "de.michab.scream.fcos.Procedure" #f))


;;
;; (apply proc arg1 ... args)  procedure; r7rs 50
;;
;; Proc must be a procedure and args must be a list. Calls proc with the
;; elements of the list (append (list arg1 ...) args) as the actual arguments.
;;
(define (apply op . list)
  (define (make-argument-list list)
    (let ((first (car list)) (rest (cdr list)))
      (if (null? rest)
        first
        (cons first (make-argument-list rest)))))
  (scream:apply op (make-argument-list list)))

;;
;; A simplified version of the map procedure.  Only able to handle a single
;; argument procedure.  Used for the implementation of the actual map operation
;; defined in the scheme standard.
;;
(define (%map1 procedure list)
  (if (null? list)
    list
    (cons
      (procedure (car list))
      (%map1 procedure (cdr list)))))



;;
;; (map proc list1 list2 ... ) R5RS32 Library procedure
;;
;; The lists must be lists, and proc must be a procedure taking as many
;; arguments as there are lists and returning a single value. If more than one
;; list is given, then they must all be the same length. Map applies proc
;; element-wise to the elements of the lists and returns a list of the results,
;; in order. The dynamic order in which proc is applied to the elements of the
;; lists is unspecified.
;;
(define (map proc . lists)
; TODO length of lists must be > 0
; TODO length of all contained lists has to be equal.
  (cond ((null? (car lists)) '())

        ((= 1 (length lists)) (%map1 proc (car lists)))

        (else (let ((car-list (%map1 car lists))
                    (cdr-list (%map1 cdr lists)))
                (cons (apply proc car-list) (apply map proc cdr-list))))))


;;
;; (for-each proc list1 list2 ... ) library procedure
;;
;; The arguments to for-each are like the arguments to map, but for-each calls
;; proc for its side effects rather than for its values. Unlike map, for-each
;; is guaranteed to call proc on the elements of the lists in order from the
;, first element(s) to the last, and the value returned by for-each is
;; unspecified.
;;
(define for-each map)

;;
;;
;;
(define (eq? x y)
    (%fco-class (eq x y)))

(define (eqv? x y)
    (%fco-class (eqv x y)))

(define (equal? x y)
    (%fco-class (equal x y)))
