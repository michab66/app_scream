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
(define (bytevector . bytes)
  (if (null? bytes)
    (make-bytevector 0)
	(make-object (de.michab.scream.fcos.Bytevector bytes))))

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
 | (bytevector-copy bytevector)  procedure; r7rs 6.9 p50
 | (bytevector-copy bytevector start)  procedure; r7rs 6.9 p50
 | (bytevector-copy bytevector start end)  procedure; r7rs 6.9 p50
 |#
(define (bytevector-copy bytevector . rest)

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

#|
 | (bytevector-copy! to at from)  procedure; r7rs 6.9 p50
 | (bytevector-copy! to at from start)  procedure; r7rs 6.9 p50
 | (bytevector-copy! to at from start end)  procedure; r7rs 6.9 p50
 |#
(define (bytevector-copy! to at from . rest)
  (if (not (bytevector? to))
    (error "TYPE_ERROR" scream:type-bytevector to))
  (if (not (bytevector? from))
    (error "TYPE_ERROR" scream:type-bytevector from))

  (define (bvc!0 to at from)
    ((object to) (copyFrom at from)))

  (define (bvc!1 to at from start)
    (cond
      ((not (integer? start))
        (error "TYPE_ERROR" scream:type-integer start))
      (else
        ((object to) (copyFrom at from start)))))

  (define (bvc!2 to at from start end)
    (cond
      ((not (integer? start))
        (error "TYPE_ERROR" scream:type-integer start))
      ((not (integer? end))
        (error "TYPE_ERROR" scream:type-integer start))
      (else
        ((object to) (copyFrom at from start end)))))

  (cond
    ((null? rest)
      (bvc!0 to at from))

    ((= 2 (length rest))
      (bvc!2 to at from (car rest) (cadr rest)))

    ((= 1 (length rest))
      (bvc!1 to at from (car rest)))

    (else
      (error "TOO_MANY_ARGUMENTS" 3))))

#|
 | (bytevector-append bytevector ...)  procedure; r7rs 6.9 p50
 |#
(define (bytevector-append . args)

  ; A local, binary, append.
  (define (bva_2 a b)
    (if (not (bytevector? a))
      (error "TYPE_ERROR" scream:type-bytevector a))
    (if (not (bytevector? b))
      (error "TYPE_ERROR" scream:type-bytevector b))
  ((object a) (append b)))

  ; Local append transitive.
  (define bva_n
    (scream:to-transitive bva_2))

  (cond
    ; No args => empty bv.
    ((null? args)
      (bytevector))
    ; One bv arg => return this.
    ((and (= 1 (length args)) (bytevector? (car args)))
      (car args))
    ; One arg, no bv => error.
    ((= 1 (length args))
      (error "TYPE_ERROR" scream:type-bytevector b))
    ; Process arg lengths >= 2.
    (else
      (apply bva_n args))))

