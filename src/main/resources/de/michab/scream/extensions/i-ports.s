;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 1998-2023 Michael G. Binz
;

;;
;; r7rs 6.13.1 p56 
;;

;;
;; Scream definitions
;;

;; The eof object.  To be reworked #78.
(define EOF 'EOF)

;; Output port type name.
(define scream:type-output-port
  ((make-object de.michab.scream.fcos.PortOut) TYPE_NAME))

;; Input port type name.
(define scream:type-input-port
  ((make-object de.michab.scream.fcos.PortIn) TYPE_NAME))

;; Predicates for the port implementation types.
(define scream:input-port?
  (typePredicateGenerator "de.michab.scream.fcos.PortIn" #t))
(define scream:binary-input-port?
  (typePredicateGenerator "de.michab.scream.fcos.PortInBinary" #t))
(define scream:output-port?
  (typePredicateGenerator "de.michab.scream.fcos.PortOut" #t))
(define scream:binary-output-port?
  (typePredicateGenerator "de.michab.scream.fcos.PortOut" #t))

;;
;; r7rs definitions.
;;

;;
;; call-with-port
;;
(define (call-with-port port proc)
  ((let ((result (proc port)))
    (close-port port)
    result)))

;; TODO call-with-input-file
;; TODO call-with-output-file

;;
;; input-port?
;;
(define (input-port? port)
  (or
    (scream:input-port? port) 
    (scream:binary-input-port? port)))
    
(define output-port?
  (typePredicateGenerator "de.michab.scream.fcos.PortOut" #t))
(define (binary-port? port)
  (if (port? port)
    ((object port) (isBinary))
    #f))
(define (textual-port? port)
  (and (port? port) (not (binary-port? port))))
(define port?
  (typePredicateGenerator "de.michab.scream.fcos.Port" #f))

;; todo input-port-open
;; todo output-port-open

;;
;; p56 
;;
;; Note that these values are not cacheable since the output
;; port may be set individually for each invocation.

(define (current-input-port)
  (scream::evaluator (getInPort)))

(define (current-output-port)
  (scream::evaluator (getOutPort)))

(define (current-error-port)
  (scream::evaluator (getErrorPort)))

;; with-input-from-file
;; with-output-to-file

;;
;; p56
;;
(define (open-input-file filename)
  (let ((in ((make-object de.michab.scream.fcos.Port) Input)))
   (make-object (de.michab.scream.fcos.Port filename in))))

;; TODO open-binary-input-file

;;
;; p56
;;
(define (open-output-file filename)
   (make-object (de.michab.scream.fcos.PortOut filename)))

;; TODO open-output-file

;;
;; p56
;;
(define (close-port port)
  (if (not (port? port))
    (error "EXPECTED_PORT"))
  ((object port) (close)))

;;
;; p56
;;
(define (close-input-port port)
  (if (not (input-port? port))
    (error "EXPECTED_INPUT_PORT"))
  (close-port port))

;;
;; p56
;;
(define (close-output-port port)
  (if (not (output-port? port))
    (error "EXPECTED_OUTPUT_PORT"))
  (close-port port))

;; todo open-input-string
;; todo open-output-string
;; todo get-output-string
;; todo open-input-bytevector
;; todo open-output-bytevector
;; todo get-output-bytevector
;;;  input ...

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

(define (eof? symbol)
    (eqv? symbol 'EOF))

(include "i-ports-output.s")
