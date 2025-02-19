;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 2023-2024 Michael G. Binz
;

;;
;; r7rs 6.9 p49
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scream definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  scream:bytevector?)

#|
 | (make-bytevector k)
 | (make-bytevector k byte) library procedure; r7rs 6.9 p49
 |#
(define make-bytevector

  (scream:delay-op (delay ; -->
    (case-lambda
  
      ((k)
        (make-object
          ("de.michab.scream.fcos.Bytevector:long"
          (scream:assert:integer 'make-bytevector k))
        )
      )
  
      ((k byte)
        (make-object
          ("de.michab.scream.fcos.Bytevector:long,long"
            (scream:assert:integer 'make-bytevector k)
            (scream:bytevector:align-to-java
              (scream:assert:integer 'make-bytevector byte)
            )
          )
        )
      )
    )
  )) ; <--
)

#| "de.michab.scream.fcos.Bytevector:byte[]"
 | (bytevector byte ...)  procedure; r7rs 6.9 p49
 |#
(define (bytevector . bytes)
  (if (null? bytes)
    (make-bytevector 0)
	(make-object (
	  "de.michab.scream.fcos.Bytevector:byte[]"
	  (scream:bytevector:align-list-to-java bytes)))))

#|
 | (bytevector-length byte ...)  procedure; r7rs 6.9 p50
 |#
(define (bytevector-length bytevector)
    ((object (scream:assert:bytevector 'bytevector-length bytevector)) ("size"))
)

#|
 | (bytevector-u8-ref bytevector k)  procedure; r7rs 6.9 p50
 |#
(define (bytevector-u8-ref bytevector k)
  (
    (object 
      (scream:assert:bytevector 'bytevector-u8-ref bytevector)
    ) 
    ("get:long" k))
)

#|
 | (bytevector-set! bytevector k)  procedure; r7rs 6.9 p50
 |#
(define (bytevector-set! bytevector k byte)
    ((object (scream:assert:bytevector 'bytevector-set! bytevector)) ("set:long,long" k byte))
)

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
      (scream:assert:bytevector 'bytevector-copy bytevector 1)
      (scream:assert:integer 'bytevector-copy start 2)
      (scream:assert:integer 'bytevector-copy end 3)
      ((object bytevector) ("copy:long,long" start end))
    )

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
      (scream:assert:bytevector 'bytevector-copy! to 1)
      (scream:assert:integer 'bytevector-copy! at 2)
      (scream:assert:bytevector 'bytevector-copy! from 3)
      (scream:assert:integer 'bytevector-copy! start 4)
      (scream:assert:integer 'bytevector-copy! end 5)
      ((object to) ("copyFrom:long,de.michab.scream.fcos.Bytevector,long,long" at from start end))
    )
  ) ; case-lambda

  )) ; <--

)

#|
 | (bytevector-append bytevector ...)  procedure; r7rs 6.9 p50
 |#
(define (bytevector-append . args)

  ; A local, binary, append.
  (define (bva_2 a b)
    (scream:assert:bytevector 'bytevector-append a)
    (scream:assert:bytevector 'bytevector-append b)
    ((object a) ("append:de.michab.scream.fcos.Bytevector" b))
  )

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
      (error "TYPE_ERROR" scream:type:bytevector b))
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
      (scream:assert:bytevector 'utf8->string bv 1)
      (scream:assert:integer 'utf8->string start 2)
      (scream:assert:integer 'utf8->string end 3)
      ((object bv) ("asString:long,long" start end))
    )
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
        (scream:assert:string 'string->utf8 string 1)
        (scream:assert:integer 'utf8->string start 2)
        (scream:assert:integer 'utf8->string end 3)
        ((object string) ("toBytevector:long,long" start end))
      )
    )
  )) ; <--
)
