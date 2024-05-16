;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 1998-2024 Michael G. Binz
;

;;
;; (vector? obj) procedure; r5rs 31
;;
(define vector?
  scream:vector?)

;;
;; (make-vector k) procedure; r7rs p48
;; (make-vector k i) procedure; r7rs 48
;;
(define (make-vector vlength . filler)
  (scream:assert:integer 'make-vector vlength 1)
  (cond
    ;; If the optional argument is not given.
    ((null? filler)
      (make-object ("de.michab.scream.fcos.Vector:long" vlength)))
    ;; If the optional argument exists.
    ((= (length filler) 1)
      (make-object ("de.michab.scream.fcos.Vector:long,de.michab.scream.fcos.FirstClassObject" vlength (car filler))))
    ;; If there are more than one optional arguments.
    (else (error "TOO_MANY_ARGUMENTS" 2)))
)

;;
;; (vector-length vector) procedure; r5rs 31
;;
(define (vector-length v)
  (scream:assert:vector 'vector-length v)
  ((object v) ("size"))
)

;;
;; (vector-ref vector k) procedure; r5rs 31
;;
(define (vector-ref v idx)
  (scream:assert:vector 'vector-ref v 1)
  (scream:assert:integer 'vector-ref idx 2)
  ((object v) ("get:long" idx))
)

;;
;; (vector-set! vector k obj) procedure; r5rs 31
;;
(define (vector-set! v idx obj)
  (scream:assert:vector 'vector-set! v 1)
  (scream:assert:integer 'vector-set! idx 2)
  ((object v) ("set:long,de.michab.scream.fcos.FirstClassObject" idx obj))
)

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
    (error "EXPECTED_PROPER_LIST"))
)

;;
;; (vector->list vector) library procedure; r5rs 31
;;
(define (vector->list v)
  (scream:assert:vector 'vector->list v 1)

  (do
    ;; Init
    ((idx (- (vector-length v) 1) (- idx 1))
     (result ()))

    ;; Test
    ((< idx 0) result)

    ;; Body
    (set! result (cons (vector-ref v idx) result))
  )
)

;;
;; (vector obj ...) library procedure; r5rs 31
;;
(define (vector . elements)
  (list->vector elements))

;;
;; (vector-fill! v filler) library procedure; r5rs 31
;;
(define (vector-fill! v filler)
  (scream:assert:vector 'vector-fill! v 1)
  ((object v) ("fill:de.michab.scream.fcos.FirstClassObject" filler))
)
