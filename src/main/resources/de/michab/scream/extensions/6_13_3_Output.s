;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 1998-2024 Michael G. Binz
;

; r7rs 6.13.3 Output p58 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scream definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; r7rs definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | (write obj)
 | (write obj port) write library procedure; r7rs 6.13.3 p58
 |#
(define write

  (scream:delay-op (delay ; -->

  (case-lambda

    ((obj)
      (write obj (current-output-port)))

    ((obj port)
      (cond
        ((not (output-port? port))
          (error "TYPE_ERROR" scream:type:output-port port))
        (else
          ((object port) ("write:de.michab.scream.fcos.FirstClassObject" obj))
          scream:unspecified)
      ) ; cond
    )

  ) ; case-lambda

  )) ; <--

)

#|
 | (write-shared obj)
 | (write-shared obj port) write library procedure; r7rs 6.13.3 p58
 |#
(define (write-shared obj port)
  (scream:error:not-implemented "(write-shared)"))

#|
 | (write-simple obj)
 | (write-simple obj port) write library procedure; r7rs 6.13.3 p59
 |#
(define (write-simple obj port)
  (scream:error:not-implemented "(write-simple)"))

#|
 | (display obj)
 | (display obj port)  write library procedure; r7rs 6.13.3 p59
 |#
(define display

  (scream:delay-op (delay ; -->

  (case-lambda

    ((obj)
      (display obj (current-output-port)))

    ((obj port)
      (cond
        ((not (output-port? port))
          (error "TYPE_ERROR" scream:type:output-port port))
        ((not (textual-port? port))
          (error "TYPE_ERROR" scream:type:output-port port))
        (else
          ((object port) ("display:de.michab.scream.fcos.FirstClassObject" obj))
          scream:unspecified)
      ) ; cond
    )

  ) ; case-lambda

  )) ; <--

)

#|
 | (newline obj)
 | (newline obj port)  procedure; r7rs 6.13.3 p59
 |#
(define newline

  (scream:delay-op (delay ; -->

  (case-lambda

    (()
      (newline (current-output-port)))

    ((port)
      
      (cond
        ((not (textual-port? port))
          (error "TYPE_ERROR" scream:type:output-port port))
        (else
          (write-char #\newline port)
          (flush-output-port port)
          scream:unspecified)
      ) ; cond
    )

  ) ; case-lambda

  )) ; <--
)

#|
 | (write-char obj)
 | (write-char obj port)  procedure; r7rs 6.13.3 p59
 |#
(define write-char

  (scream:delay-op (delay ; -->

  (case-lambda

    ((character)
      (write-u8 character (current-output-port)))

    ((character port)
      (cond
        ((not (char? character))
          (error "TYPE_ERROR" scream:type-character character))
        ((not (output-port? port))
          (error "TYPE_ERROR" scream:type:output-port port))
        (else
          ((object port) ("display:de.michab.scream.fcos.FirstClassObject" character))
          scream:unspecified)
      ) ; cond
    )

  ) ; case-lambda

  )) ; <--

)

#|
 | (write-string obj)
 | (write-string obj port)
 | (write-string obj port start)
 | (write-string obj port start end)   procedure; r7rs 6.13.3 p59
 |#
(define write-string

  (scream:delay-op (delay ; -->

  (case-lambda

    ((string)
      (write-string string (current-output-port) 0 (string-length string)))

    ((string port)
      (write-string string port 0 (string-length string)))

    ((string port start)
      (write-string string port start (string-length string)))

    ((string port start end)
      (cond
        ((not (string? string))
          (error "TYPE_ERROR" scream:type:string string))
        ((not (output-port? port))
          (error "TYPE_ERROR" scream:type:output-port port))
        ((not (textual-port? port))
          (error "TYPE_ERROR" scream:textual-port port))
        ((not (integer? start))
          (error "TYPE_ERROR" scream:integer start))
        ((not (integer? end))
          (error "TYPE_ERROR" scream:integer end))
        (else
          ((object port) ("display:de.michab.scream.fcos.FirstClassObject" (substring string start end)))
          scream:unspecified)
      ) ; cond
    )

  ) ; case-lambda

  )) ; <--
)

#|
 | (write-u8 obj)
 | (write-u8 obj port)  procedure; r7rs 6.13.3 p59
 |#
(define write-u8

  (scream:delay-op (delay ; -->

  (case-lambda

    ((byte)
      (write-u8 byte (current-output-port)))

    ((byte port)
      (cond
        ((not (integer? byte))
          (error "TYPE_ERROR" scream:type-integer byte))
        ((not (output-port? port))
          (error "TYPE_ERROR" scream:type:output-port port))
        ((not (binary-port? port))
          (error "TYPE_ERROR" scream:binary-port port))
        (else
          ((object port) ("writeByte:de.michab.scream.fcos.Int" byte))
          scream:unspecified)
      ) ; cond
    )

  ) ; case-lambda

  )) ; <--

)

#|
 | (write-bytevector obj)
 | (write-bytevector obj port)
 | (write-bytevector obj port start)
 | (write-bytevector obj port start end)  procedure; r7rs 6.13.3 p59
 |#
(define write-bytevector

  (scream:delay-op (delay ; -->

  (case-lambda

    ((bytevector port)
      (write-bytevector bytevector port 0 (bytevector-length bytevector)))

    ((bytevector port start)
      (write-bytevector bytevector port start (bytevector-length bytevector)))

    ((bytevector port start end)
      (cond
        ((not (bytevector? bytevector))
          (error "TYPE_ERROR" scream:type-bytevector bytevcetor))
        ((not (output-port? port))
          (error "TYPE_ERROR" scream:type:output-port port))
        ((not (binary-port? port))
          (error "TYPE_ERROR" scream:binary-port port))
        (else
          (let ((buffer ((object bytevector) ("copy:long,long" start end))))
            ((object port) ("write:byte[]" ((object buffer) ("toJava"))))
            scream:unspecified))
      ) ; cond
    )

  ) ; case-lambda

  )) ; <--

)

#|
 | (flush-output-port)
 | (flush-output-port port)  procedure; r7rs 6.13.3 p59
 |#
(define flush-output-port

  (scream:delay-op (delay ; -->

  (case-lambda

    (()
      (flush-output-port (current-output-port)))

    ((port)
      (cond
        ((not (output-port? port))
          (error "TYPE_ERROR" scream:type:output-port port))
        (else
          ((object port) ("flush:"))
          scream:unspecified)
      ) ; cond
    )

  ) ; case-lambda

  )) ; <--
)
