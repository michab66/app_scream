;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 1998-2023 Michael G. Binz
;

;;
;; r7rs 6.13.3 Output p58 
;;

;;
;; p58
;;
;; (write obj) library procedure
;; (write obj port) library procedure
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
    ((object the-port) (write subject))))

;;
;; TODO write-shared p58
;; TODO write-simple p58
;;

;;
;; p59
;;
;; (display obj)
;; (display obj port)
;;
(define (display subject . arg-list)
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
    ; Finally do the actual write.
    ((object the-port) (display subject))))

;;
;; p59
;;
;; (newline)
;; (newline port)
;;
;; Writes an end of line to port. Exactly how this is done differs from one
;; operating system to another. Returns an unspecified value. The port argument
;; may be omitted, in which case it defaults to the value returned by
;; current-output-port.
;;
(define (newline . opt-port)
  (let (
    ; If a single optional argument is given assign this to the port.  If no
    ; optional argument is given assign the current output port to the port.
    ; If more than one optional argument is given return an error.
    (the-port
      (cond
        ((= 0 (length opt-port))
          (current-output-port))
        ((= 1 (length opt-port))
          (car opt-port))
        (else
          (error "TOO_MANY_ARGUMENTS" 2)))))

  (write-char #\newline the-port)))

;;
;; p59
;;
;; (write-char char) procedure
;; (write-char char port) procedure
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


;; (write-string string)
;; (write-u8 byte)
;; (write-bytevector)
;; (flush-output-port)
