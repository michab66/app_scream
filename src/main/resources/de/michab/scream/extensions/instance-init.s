;
; Scream @ https://github.com/michab/dev_smack
;
; Copyright © 1998-2023 Michael G. Binz
;

;;
;; Access the environment user expressions are evaluated in.
;;
(define (interaction-environment)
  (%%interpreter%% (getTopLevelEnvironment)))

(define micbinz 313)

(load "i-test.s")

;;
;; Returns the next character available from the input port, without updating
;; the port to point to the following character.  If no more characters are
;; available, an end of file object is returned. Port may be omitted, in which
;; case it defaults to the value returned by current-input-port.
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
;; Returns #t if a character is ready on the input port and returns #f
;; otherwise.  If char-ready returns #t then the next read-char operation on
;; the given port is guaranteed not to hang. If the port is at end of file then
;; char-ready? returns #t. Port may be omitted, in which case it defaults to
;; the value returned by current-input-port.
;;
(define (char-ready . arg-list)
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
;; (write obj),      library procedure, r5rs 36
;; (write obj port), library procedure, r5rs 36
;;
;; Writes a written representation of obj to the given port. Strings that
;; appear in the written representation are enclosed in doublequotes, and
;; within those strings backslash and doublequote characters are escaped by
;; backslashes.
;; Character objects are written using the #\ notation. Write returns an
;; unspecified value. The port argument may be omitted, in which case it
;; defaults to the value returned by current-output-port.
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
;; Writes the character char (not an external representation of the character)
;; to the given port and returns an unspecified value. The port argument may be
;; omitted, in which case it defaults to the value returned by
;; current-output-port.
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
;; (load string),      library procedure, r5rs 37
;;
;; Filename should be a string naming an existing file containing Scheme source
;; code. The load procedure reads expressions and definitions from the file and
;; evaluates them sequentially. It is unspecified whether the results of the
;; expressions are printed. The load procedure does not affect the values
;; returned by current-input-port and current-output-port. Load returns an
;; unspecified value.
;;
(define (load string)
  (%%interpreter%% (load string)))


(define (eof? symbol)
    (eqv? symbol 'EOF))
