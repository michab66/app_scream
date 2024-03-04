; Scream @ https://github.com/urschleim/scream
;
; Copyright © 2023-2024 Michael G. Binz

;;
;; r7rs 6.9 p49
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scream definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Init type name.
 |#
(define scream:type-bytevector
  ((make-object "de.michab.scream.fcos.Bytevector") TYPE_NAME))

#|
 | (scream-bytevector:align-to-java byte)
 | Performs a byte range-check when it is passed via reflection.
 | Bytes [-128..127] are returned unchanged.
 | Byte values [127..255] are mapped to [-128..-1].
 | All other values result in an RANGE_EXCEEDED error.
 |#
(define (scream:bytevector:align-to-java byte)
  (cond
    ((> byte 255)
      (error "RANGE_EXCEEDED" byte "[-127..255]"))
    ((< byte -128)
      (error "RANGE_EXCEEDED" byte "[-127..255]"))
    ((> byte 127)
      (- byte 256))
    (else
      byte)
  )
)

#|
 |
 |#
(define (scream:bytevector:align-list-to-java bytes)
  (map 
    scream:bytevector:align-to-java
    bytes
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; r7rs definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | (bytevector? obj) library procedure; 6.9 r7rs 49
 |#
(define bytevector?
  (typePredicateGenerator "de.michab.scream.fcos.Bytevector" #t))

#|
 | (make-bytevector k)
 | (make-bytevector k byte) library procedure; r7rs 6.9 p49
 |#
(define (make-bytevector k . byte)
  (if (not (integer? k))
    (error "TYPE_ERROR" k scream:type-integer))

  (cond
    ;; If the optional argument is not given.
    ((null? byte)
      (make-object ("de.michab.scream.fcos.Bytevector:long" k)))
    ;; If the optional argument exists.
    ((= (length byte) 1)
      (let ((byte (car byte)))
        (cond
          ((not (integer? byte))
             (error "TYPE_ERROR" byte scream:type-integer))
          (else
             (make-object (
               "de.michab.scream.fcos.Bytevector:long,long"
               k
               (scream:bytevector:align-to-java byte))))
        )
      ))
    ;; If there is more than one optional argument.
    (else (error "TOO_MANY_ARGUMENTS" 2))
  ) ; cond
)

#| "de.michab.scream.fcos.Bytevector:byte[]"
 | (bytevector byte ...)  procedure; r7rs 6.9 p49
 |#
(if #f
(define (bytevector . bytes)
  (if (null? bytes)
    (make-bytevector 0)
	(make-object (de.michab.scream.fcos.Bytevector bytes))))
(define (bytevector . bytes)
  (if (null? bytes)
    (make-bytevector 0)
	(make-object (
	  "de.michab.scream.fcos.Bytevector:byte[]"
	  (scream:bytevector:align-list-to-java bytes)))))
)	

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
(define bytevector-copy

  (scream:delay-op (delay ; -->

  (case-lambda

    ((bytevector)
      (bytevector-copy bytevector 0 (bytevector-length bytevector)))

    ((bytevector start)
      (bytevector-copy bytevector start (bytevector-length bytevector)))

    ((bytevector start end)
      (cond
        ((not (bytevector? bytevector))
          (error "TYPE_ERROR" scream:type-bytevector bytevector))
        ((not (integer? start))
          (error "TYPE_ERROR" scream:type-integer start))
        ((not (integer? end))
          (error "TYPE_ERROR" scream:type-integer start))
        (else
          ((object bytevector) (copy start end)))))

  ) ; case-lambda

  )) ; <--
)    

#|
 | (bytevector-copy! to at from)  procedure; r7rs 6.9 p50
 | (bytevector-copy! to at from start)  procedure; r7rs 6.9 p50
 | (bytevector-copy! to at from start end)  procedure; r7rs 6.9 p50
 |#
(define bytevector-copy!

  (scream:delay-op (delay ; -->

  (case-lambda

    ((to at from)
      (bytevector-copy! to at from 0 (bytevector-length from)))

    ((to at from start)
      (bytevector-copy! to at from start (bytevector-length from)))

    ((to at from start end)
      (cond
        ((not (bytevector? to))
          (error "TYPE_ERROR" scream:type-bytevector to))
        ((not (bytevector? from))
          (error "TYPE_ERROR" scream:type-bytevector from))
        ((not (integer? start))
          (error "TYPE_ERROR" scream:type-integer start))
        ((not (integer? end))
          (error "TYPE_ERROR" scream:type-integer end))
        (else
          ((object to) (copyFrom at from start end)))))
  ) ; case-lambda

  )) ; <--

)

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

#|
 | (utf8->string bytevector)  procedure; r7rs 6.9 p50
 | (utf8->string bytevector start)  procedure; r7rs 6.9 p50
 | (utf8->string bytevector start end)  procedure; r7rs 6.9 p50
 |#
(define utf8->string
  (scream:delay-op (delay ; -->

  (case-lambda

    ((bv)
      (utf8->string
        bv
        0
        (bytevector-length bv)))

    ((bv start)
      (utf8->string
        bv
        start
        (bytevector-length bv)))

    ((bv start end)
      (cond
        ((not (bytevector? bv))
          (error "TYPE_ERROR" scream:type-bytevector bv))
        ((not (integer? start))
          (error "TYPE_ERROR" scream:type-integer start))
        ((not (integer? end))
          (error "TYPE_ERROR" scream:type-integer end))
        (else
          ((object bv) (asString start end)))))
  )
  )) ; <--
)

#|
 | (string->utf8 string)  procedure; r7rs 6.9 p50
 | (string->utf8 string start)  procedure; r7rs 6.9 p50
 | (string->utf8 string start end)  procedure; r7rs 6.9 p50
 |#
(define string->utf8

  (scream:delay-op (delay ; -->
    (case-lambda
  
      ((string)
        (string->utf8
           string
           0
           (string-length string)))
  
      ((string start)
        (string->utf8
          string
          start
          (string-length string)))

      ((string start end)
       (cond
         ((not (string? string))
           (error "TYPE_ERROR" scream:type-string string))
         ((not (integer? start))
           (error "TYPE_ERROR" scream:type-integer start))
         ((not (integer? end))
           (error "TYPE_ERROR" scream:type-integer end))
         (else
           ((object string) (toBytevector start end))))))
  )) ; <--
)
