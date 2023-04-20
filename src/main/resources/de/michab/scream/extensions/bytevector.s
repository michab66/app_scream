; Scream @ https://github.com/urschleim/scream
;
; Copyright © 2023 Michael G. Binz

;;
;; r7rs 6.9 p49
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scream definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Init type name.
(define scream:type-bytevector
  ((make-object de.michab.scream.fcos.Bytevector) TYPE_NAME))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; r7rs definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; (bytevector? obj) library procedure; 6.9 r7rs 49
;;
(define bytevector?
  (typePredicateGenerator "de.michab.scream.fcos.Bytevector" #t))

#|
 | (make-bytevector k)
 | (make-bytevector k byte) library procedure; r7rs 6.9 p49
 |#

(define scream:int-max #x7fffffff)

(define (make-bytevector k . byte)
  (if (not (integer? k))
    (error "TYPE_ERROR" k scream:type-integer))

  (cond
    ;; If the optional argument is not given.
    ((null? byte)
      (make-object (de.michab.scream.fcos.Bytevector k)))
    ;; If the optional argument exists.
    ((= (length byte) 1)
      (let ((byte (car byte)))
        (cond
          ((not (integer? byte))
             (error "TYPE_ERROR" byte scream:type-integer))
          (else
             (make-object (de.michab.scream.fcos.Bytevector k byte)))
        )
      ))
    ;; If there are more than one optional arguments.
    (else (error "TOO_MANY_ARGUMENTS" 2))
  ) ; cond
)

#|
 | (bytevector byte ...)  procedure; r7rs 6.9 p49
 |#
(define (bytevector . byte)
	(make-object (de.michab.scream.fcos.Bytevector byte)))

#|
 | (bytevector-length byte ...)  procedure; r7rs 6.9 p50
 |#
(define (bytevector-length bytevector)
  (if (not (bytevector? bytevector))
    (error "TYPE_ERROR" scream:type-bytevector bytevector)
    ((object bytevector) (size))))

#|
 | (bytevector-ref bytevector k)  procedure; r7rs 6.9 p50
 |#
(define (bytevector-ref bytevector k)
  (if (not (bytevector? bytevector))
    (error "TYPE_ERROR" scream:type-bytevector bytevector)
    ((object bytevector) (get k))))

#|
 | (bytevector-set! bytevector k)  procedure; r7rs 6.9 p50
 |#
(define (bytevector-set! bytevector k byte)
  (if (not (bytevector? bytevector))
    (error "TYPE_ERROR" scream:type-bytevector bytevector)
    ((object bytevector) (set k byte))))

#|
 | (bytevector-copy bytevector bytevector)  procedure; r7rs 6.9 p50
 | (bytevector-copy bytevector bytevector start)  procedure; r7rs 6.9 p50
 | (bytevector-copy bytevector bytevector start end)  procedure; r7rs 6.9 p50
 |#
 
(define (bvc0 bv)
  ((object bv) (copy)))

(define (bvc1 bv start)
  (cond
    ((not (integer? start))
      (error "TYPE_ERROR" scream:type-integer start))
    (else
      ((object bv) (copy start)))))

(define (bvc2 bv start end)
  (cond
    ((not (integer? start))
      (error "TYPE_ERROR" scream:type-integer start))
    ((not (integer? end))
      (error "TYPE_ERROR" scream:type-integer start))
    (else
      ((object bv) (copy start end)))))

(define (bytevector-copy bytevector . rest)
  (if (not (bytevector? bytevector))
    (error "TYPE_ERROR" scream:type-bytevector bytevector)

    (cond
      ((null? rest)
        (bvc0 bytevector))

      ((= 2 (length rest))
        (bvc2 bytevector (car rest) (cadr rest)))

      ((= 1 (length rest))
        (bvc1 bytevector (car rest)))

      (else
        (error "TOO_MANY_ARGUMENTS" 3)))))
