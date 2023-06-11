;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 1998-2023 Michael G. Binz
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; r7rs 6.13.3 Output p58 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; write write library procedure
;;
(define (write subject . arg-list)
  (let (
    ; If a single optional argument is given assign this to the port.  If no
    ; optional argument is given assign the current output port to the port.
    ; If more than one optional argument is given return an error.
    (the-port
      (cond
        ((= 0 (length arg-list))
          (current-output-port))
        ((= 1 (length arg-list))
          (car arg-list))
        (else
          (error "TOO_MANY_ARGUMENTS" 2)))))

    ; Check if what we assigned above is really an output port.
    (if (not (port? the-port))
      (error "TYPE_ERROR" %type-port (%typename the-port) 2))
    (if (not (output-port? the-port))
      (error "EXPECTED_OUTPUT_PORT"))
    ; Finally do the actual read.
    ((object the-port) (write subject))
    scream:unspecified))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; write-shared write library procedure
;;
(define (write-shared obj port)
  (scream:error:not-implemented "(write-shared)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; write-simple write library procedure
;;
(define (write-simple obj port)
  (scream:error:not-implemented "(write-simple)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; display write library procedure
;;
(define display

  (scream:delay-op (delay ; -->

  (case-lambda

    ((obj)
      (display obj (current-output-port)))

    ((obj port)
      (cond
        ((not (output-port? port))
          (error "TYPE_ERROR" scream:type-output-port port))
        ((not (textual-port? port))
          (error "TYPE_ERROR" scream:type-output-port port))
        (else
          ((object port) (display obj))
          scream:unspecified)
      ) ; cond
    )

  ) ; case-lambda

  )) ; <--

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; newline procedure
;;
(define newline

  (scream:delay-op (delay ; -->

  (case-lambda

    (()
      (newline (current-output-port)))

    ((port)
      
      (cond
        ((not (output-port? port))
          (error "TYPE_ERROR" scream:type-output-port port))
        ((not (textual-port? port))
          (error "TYPE_ERROR" scream:type-output-port port))
        (else
          (write-char #\newline port)
          scream:unspecified)
      ) ; cond
    )

  ) ; case-lambda

  )) ; <--
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; write-char procedure
;;
(define (write-char char . arg-list)
  (let (
    ; If a single optional argument is given assign this to the port.  If no
    ; optional argument is given assign the current output port to the port.
    ; If more than one optional argument is given return an error.
    (the-port
      (cond
        ((= 0 (length arg-list))
          (current-output-port))
        ((= 1 (length arg-list))
          (car arg-list))
        (else
          (error "TOO_MANY_ARGUMENTS" 2)))))

    ; Check if what we assigned above is really an output port.
    (if (not (port? the-port))
      (error "TYPE_ERROR" %type-port (%typename the-port) 2))
    (if (not (output-port? the-port))
      (error "EXPECTED_OUTPUT_PORT"))
    (if (not (char? char))
      (error "TYPE_ERROR" %type-char (%typename char) 1))
    ; Finally do the actual write.
    ((object the-port) (writeCharacter char))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; write-string procedure
;;
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
          (error "TYPE_ERROR" scream:type-string string))
        ((not (output-port? port))
          (error "TYPE_ERROR" scream:type-output-port port))
        ((not (textual-port? port))
          (error "TYPE_ERROR" scream:textual-port port))
        ((not (integer? start))
          (error "TYPE_ERROR" scream:integer start))
        ((not (integer? end))
          (error "TYPE_ERROR" scream:integer end))
        (else
          ((object port) (display (substring string start end)))
          scream:unspecified)
      ) ; cond
    )

  ) ; case-lambda

  )) ; <--
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; write-u8 procedure
;;
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
          (error "TYPE_ERROR" scream:type-output-port port))
        ((not (binary-port? port))
          (error "TYPE_ERROR" scream:binary-port port))
        (else
          ((object port) (writeByte byte))
          scream:unspecified)
      ) ; cond
    )

  ) ; case-lambda

  )) ; <--

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; write-bytevector procedure
;;
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
          (error "TYPE_ERROR" scream:type-output-port port))
        ((not (binary-port? port))
          (error "TYPE_ERROR" scream:binary-port port))
        (else
          (let ((buffer ((object bytevector) (copy start end))))
            ((object port) (write ((object buffer) (toJava))))
            scream:unspecified))
      ) ; cond
    )

  ) ; case-lambda

  )) ; <--

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flush-output-port procedure
;;
(define flush-output-port

  (scream:delay-op (delay ; -->

  (case-lambda

    (()
      (flush-output-port (current-output-port)))

    ((port)
      (cond
        ((not (output-port? port))
          (error "TYPE_ERROR" scream:type-output-port port))
        (else
          ((object port) (flush))
          scream:unspecified)
      ) ; cond
    )

  ) ; case-lambda

  )) ; <--
)
