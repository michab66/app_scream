; $Id: vector.s 8 2008-09-14 14:23:20Z binzm $
;
; Scream / Runtime
;
; Released under Gnu Public License
; Copyright (c) 1998-2000 Michael G. Binz



;;
;; (vector? obj) procedure; r5rs 31
;;
(define vector?
  (typePredicateGenerator "de.michab.scream.fcos.Vector" #t))



;;
;; (make-vector k) procedure; r5rs 31
;; (make-vector k i) procedure; r5rs 31
;;
(define (make-vector vlength . filler)
  (cond
    ;; If the optional argument is not given.
    ((null? filler)
      (make-object ("de.michab.scream.fcos.Vector:long" vlength)))
    ;; If the optional argument exists.
    ((= (length filler) 1)
      (make-object ("de.michab.scream.fcos.Vector:long,de.michab.scream.fcos.FirstClassObject" vlength (car filler))))
    ;; If there are more than one optional arguments.
    (else (error "TOO_MANY_ARGUMENTS" 2))))



;;
;; (vector-length vector) procedure; r5rs 31
;;
(define (vector-length v)
  (if (vector? v)
    ((object v) (size))
    (error "TYPE_ERROR" %type-vector (scream:typename v))))



;;
;; (vector-ref vector k) procedure; r5rs 31
;;
(define (vector-ref v idx )
  (if (vector? v)
    ((object v) (get idx))
    (error "TYPE_ERROR" %type-vector (scream:typename v) 1)))



;;
;; (vector-set! vector k obj) procedure; r5rs 31
;;
(define (vector-set! v idx obj)
  (if (vector? v)
    ((object v) (set idx obj))
    (error "TYPE_ERROR" %type-vector (scream:typename v) 1)))



;;
;; (list->vector list) library procedure; r5rs 31
;;
(define (list->vector list)
  (if (list? list)
    (do
      ;; Init
      ;; Create the vector to return in the end.
      ((result (make-vector (length list)))
      ;; The vector index to set.
       (idx 0 (+ idx 1))
      ;; This holds the list of elements to be moved.
       (rest-to-move list (cdr rest-to-move)))

      ;; Test
      ;; Check if there are elements to be moved.
      ((null? rest-to-move) result)

      ;; Body
      (vector-set! result idx (car rest-to-move)))
    (error "EXPECTED_PROPER_LIST")))



;;
;; (vector->list vector) library procedure; r5rs 31
;;
(define (vector->list v)
  (if (vector? v)
    (do
      ;; Init
      ((idx (- (vector-length v) 1) (- idx 1))
       (result ()))

      ;; Test
      ((< idx 0) result)

      ;; Body
      (set! result (cons (vector-ref v idx) result)))

    (error "TYPE_ERROR" %type-vector (scream:typename v))))



;;
;; (vector obj ...) library procedure; r5rs 31
;;
(define (vector . elements)
  (list->vector elements))



;;
;; (vector-fill! v filler) library procedure; r5rs 31
;;
(define (vector-fill! v filler)
  (if (vector? v)
    ((object v) (fill filler))
    (error "TYPE_ERROR" %type-vector (scream:typename v) 1)))
