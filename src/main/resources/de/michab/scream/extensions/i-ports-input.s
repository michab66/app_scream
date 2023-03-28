;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 1998-2023 Michael G. Binz
;

;;
;; scream specific
;;

;; The eof object.  To be reworked #78.
(define EOF 'EOF)

;;
;; r7rs 6.13.2 Input p57
;;

;;
;; read       read library procedure
;;
(define (read . arg-list)
  ;; TODO Note that this has a problem.  When reading from standard input
  ;; sometimes more than one expression has to be specified for read to
  ;; return.  This is exactly the same behavior that the implementation in
  ;; native Java had.
  (let (
    ; If a single argument is given assign this to the port.  If no argument
    ; is given assign the current input port to the port.  If than one
    ; argument is given return an error.
    (the-port
      (cond
        ((= 0 (length arg-list))
          (current-input-port))
        ((= 1 (length arg-list))
          (car arg-list))
        (else
          (error "TOO_MANY_ARGUMENTS" 1)))))

    ; Check if what we assigned above is really an input port.
    (if (not (port? the-port))
      (error "TYPE_ERROR" %type-port (%typename the-port) 2))
    (if (not (input-port? the-port))
      (error "EXPECTED_INPUT_PORT"))
    ; Finally do the actual read.
    ((object the-port) (read))))

;;
;; read-char procedure
;;
(define (read-char . arg-list)
  (let (
    ; If a single argument is given assign this to the port.  If no argument
    ; is given assign the current input port to the port.  If than one
    ; argument is given return an error.
    (the-port
      (cond
        ((= 0 (length arg-list))
          (current-input-port))
        ((= 1 (length arg-list))
          (car arg-list))
        (else
          (error "TOO_MANY_ARGUMENTS" 1)))))

    (if (not (port? the-port))
      (error "TYPE_ERROR" %type-port (%typename the-port) 2))
    (if (not (input-port? the-port))
      (error "EXPECTED_INPUT_PORT"))
    ; Finally do the actual read.
    ((object the-port) (readCharacter))))

;;
;; peek-char procedure
;;
(define (peek-char . arg-list)
  (let (
    ; If a single argument is given assign this to the port.  If no argument
    ; is given assign the current input port to the port.  If than one
    ; argument is given return an error.
    (the-port
      (cond
        ((= 0 (length arg-list))
          (current-input-port))
        ((= 1 (length arg-list))
          (car arg-list))
        (else
          (error "TOO_MANY_ARGUMENTS" 1)))))

    (if (not (port? the-port))
      (error "TYPE_ERROR" %type-port (%typename the-port) 2))
    (if (not (input-port? the-port))
      (error "EXPECTED_INPUT_PORT"))
    ; Finally do the actual read.
    ((object the-port) (peekCharacter))))

;;
;; read-line procedure
;;
(define (read-line port)
  (scream:error:not-implemented "(read-line)"))

;;
;; eof-object? procedure
;;
;; TODO
(define (eof-object? obj)
  (eq? 'EOF obj))

;;
;; eof-object procedure
;;
;; TODO
(define (eof-object) 'EOF)

;;
;; char-ready? procedure
;;
(define (char-ready? . arg-list)
  (let (
    ; If a single argument is given assign this to the port.  If no argument
    ; is given assign the current input port to the port.  If than one
    ; argument is given return an error.
    (the-port
      (cond
        ((= 0 (length arg-list))
          (current-input-port))
        ((= 1 (length arg-list))
          (car arg-list))
        (else
          (error "TOO_MANY_ARGUMENTS" 1)))))

    (if (not (port? the-port))
      (error "TYPE_ERROR" %type-port (%typename the-port) 2))
    (if (not (input-port? the-port))
      (error "EXPECTED_INPUT_PORT"))
    ; Finally do the actual read.
    ((object the-port) (charReady))))

;;
;; read-string procedure
;;
(define (read-string k port)
  (scream:error:not-implemented "(read-string)"))

;;
;; read-u8 procedure
;;
(define (read-u8 port)
  (scream:error:not-implemented "(read-u8)"))

;;
;; peek-u8 procedure
;;
(define (peek-u8 port)
  (scream:error:not-implemented "(read-u8)"))

;;
;; u8-ready? procedure
;;
(define (u8-ready? . port)
  (scream:error:not-implemented "(u8-ready?)"))

;;
;; read-bytevector procedure
;;
(define (read-bytevector k port)
  (scream:error:not-implemented "(read-bytevector)"))

;;
;; read-bytevector! procedure
;;
(define (read-bytevector! bytevector port start end)
  (scream:error:not-implemented "(read-bytevector!)"))
