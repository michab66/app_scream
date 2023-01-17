; $Id: instance-init.s 196 2009-08-03 20:45:06Z Michael $
;
; Scream / Common extensions
;
; Released under Gnu Public License
; Comments contain material from R5RS.
;
; Copyright (c) 1998-2008 Michael G. Binz



;;
;; Access the environment user expressions are evaluated in.
;;
(define (interaction-environment)
  (%%interpreter%% (getTopLevelEnvironment)))



;;
;; Get a reference to the default input port a.k.a. as standard in.
;;
(define (current-input-port) (%%interpreter%% (getInPort)))



;;
;; Get a reference to the default output port a.k.a. as standard out.
;;
(define (current-output-port) (%%interpreter%% (getOutPort)))



;;
;; Takes a string naming an existing file and returns an input port capable of
;; delivering characters from the file. If the file cannot be opened, an error
;; is signalled.
;;
(define (open-input-file filename)
  (let ((in ((make-object de.michab.scream.fcos.Port) Input)))
   (make-object (de.michab.scream.fcos.Port filename in))))



;;
;; Closes the file associated with port, rendering the port incapable of
;; delivering characters. This routine has no effect if the file has already
;; been closed. The value returned is unspecifed.
;;
(define (close-input-port port)
  (if (not (input-port? port))
    (error "EXPECTED_INPUT_PORT"))
  ((object port) (close)))



;;
;; Takes a string naming an output file to be created and returns an output
;; port capable of writing characters to a new file by that name.  If the
;; file cannot be opened, an error is signalled.  If a file with the given name
;; already exists, the effect is unspecified.
;;
(define (open-output-file filename)
  (let ((out ((make-object de.michab.scream.fcos.Port) Output)))
   (make-object (de.michab.scream.fcos.Port filename out))))



;;
;; Closes the file associated with port, rendering the port incapable of
;; accepting characters.  This routine has no effect if the file has already
;; been closed.  The value returned is unspecified.
;;
(define (close-output-port port)
  (if (not (output-port? port))
    (error "EXPECTED_OUTPUT_PORT"))
  ((object port) (close)))



;;
;; Writes a representation of obj to the given port. Strings that appear in the
;; written representation are not enclosed in doublequotes, and no characters
;; are escaped within those strings.  Character objects appear in the
;; representation as if written by write-char instead of by write.
;; Display returns an unspecified value.  The port argument may be omitted, in
;; which case it defaults to the value returned by current-output-port.
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
    ; Finally do the actual read.
    ((object the-port) (display subject))))



;;
;; Should be hidden in a closure.  No user access allowed.
;; (%port? obj) procedure; r5rs 29
;;
(define port?
  (typePredicateGenerator "de.michab.scream.fcos.Port" #t))



;;
;; Returns #t if obj is an input port, otherwise returns #f.
;;
(define (input-port? port)
  (if (port? port)
    ((object port) (isInputPort))
    #f))



;;
;; Returns #t if obj is an output port, otherwise returns #f.
;;
(define (output-port? port)
  (if (port? port)
    ((object port) (isOutputPort))
    #f))



;;
;; (read)       library procedure, r5rs 36
;; (read port)  library procedure, r5rs 36
;;
;; Read converts external representations of Scheme objects into the objects
;; themselves. That is, it is a parser for the nonterminal 'datum' (see R5RS
;; sections 7.1.2 and 6.3.2). Read returns the next object parsable from the
;; given input port, updating port to point to the first character past the end
;; of the external representation of the object.
;; If an end of file is encountered in the input before any characters are
;; found that can begin an object, then an end of file object is returned. The
;; port remains open, and further attempts to read will also return an end of
;; file object.
;; If an end of file is encountered after the beginning of an object's external
;; representation, but the external representation is incomplete and therefore
;; not parsable, an error is signalled.
;; The port argument may be omitted, in which case it defaults to the value
;; returned by current-input-port. It is an error to read from a closed port.
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
;; Returns the next character available from the input port, updating the port
;; to point to the following character. If no more characters are available, an
;; end of file object is returned. Port may be omitted, in which case it
;; defaults to the value returned by current-input-port.
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
