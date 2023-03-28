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
;; call-with-port procedure
;;
(define (call-with-port port proc)
  ((let ((result (proc port)))
    (close-port port)
    result)))

;;
;; call-with-input-file file library procedure
;;
(define (call-with-input-file string proc)
  (scream:error:not-implemented "(call-with-input-file)"))

;;
;; call-with-output-file file library procedure
;;
(define (call-with-output-file string proc)
  (scream:error:not-implemented "(call-with-output-file)"))

;;
;; input-port? procedure
;;
(define (input-port? port)
  (or
    (scream:input-port? port) 
    (scream:binary-input-port? port)))

;;
;; output-port? procedure
;;
(define output-port?
  (typePredicateGenerator "de.michab.scream.fcos.PortOut" #t))

;;
;; binary-port? procedure
;;
(define (binary-port? port)
  (if (port? port)
    ((object port) (isBinary))
    #f))

;;
;; textual-port? procedure
;;
(define (textual-port? port)
  (and (port? port) (not (binary-port? port))))

;;
;; port? procedure
;;
(define port?
  (typePredicateGenerator "de.michab.scream.fcos.Port" #f))

;;
;; input-port-open? procedure
;;
(define (input-port-open? port)
  (if (input-port? port)
    (not ((object port) (isClosed)))
    #f))

;;
;; output-port-open? procedure
;;
(define (output-port-open? port)
  (if (output-port? port)
    (not ((object port) (isClosed)))
    #f))

;; Note that these values are not cacheable since the output
;; port may be set individually for each invocation.

;;
;; current-input-port procedure
;;
;; Note that the value is not cacheable since the
;; port may be set individually for each invocation.
(define (current-input-port)
  (scream:evaluator (getInPort)))

;;
;; current-output-port procedure
;;
;; Note that the value is not cacheable since the
;; port may be set individually for each invocation.
(define (current-output-port)
  (scream:evaluator (getOutPort)))

;;
;; current-error-port procedure
;;
;; Note that the value is not cacheable since the
;; port may be set individually for each invocation.
(define (current-error-port)
  (scream:evaluator (getErrorPort)))

;;
;; with-input-from-file file library procedure
;;
(define (with-input-from-file string thunk)
  (scream:error:not-implemented "(with-input-from-file)"))

;;
;; with-output-to-file file library procedure
;;
(define (with-output-to-file string thunk)
  (scream:error:not-implemented "(with-output-to-file)"))

;;
;; open-input-file file library procedure
;;
(define (open-input-file filename)
  (let ((in ((make-object de.michab.scream.fcos.Port) Input)))
   (make-object (de.michab.scream.fcos.Port filename in))))

;;
;; open-binary-input-file file library procedure
;;
(define (open-binary-input-file filename)
  (scream:error:not-implemented "(open-binary-input-file)"))

;;
;; open-output-file file library procedure
;;
(define (open-output-file filename)
   (make-object (de.michab.scream.fcos.PortOut filename)))

;;
;; open-binary-output-file file library procedure
;;
(define (open-binary-output-file filename)
  (scream:error:not-implemented "(open-binary-output-file)"))

;;
;; close-port  procedure
;;
(define (close-port port)
  (if (not (port? port))
    (error "EXPECTED_PORT"))
  ((object port) (close)))

;;
;; close-input-port procedure
;;
(define (close-input-port port)
  (if (not (input-port? port))
    (error "EXPECTED_INPUT_PORT"))
  (close-port port))

;;
;; close-output-port  procedure
;;
(define (close-output-port port)
  (if (not (output-port? port))
    (error "EXPECTED_OUTPUT_PORT"))
  (close-port port))

;;
;; open-input-string procedure
;;
(define (open-input-string string)
  (scream:error:not-implemented "(open-input-string)"))

;;
;; open-output-string procedure
;;
(define (open-output-string)
  (scream:error:not-implemented "(open-output-string)"))

;;
;; get-output-string procedure
;;
(define (get-output-string port)
  (scream:error:not-implemented "(get-output-string)"))

;;
;; open-input-bytevector procedure
;;
(define (open-input-bytevector bytevector)
  (scream:error:not-implemented "(open-input-bytevector)"))

;;
;; open-output-bytevector procedure
;;
(define (open-output-bytevector)
  (scream:error:not-implemented "(open-output-bytevector)"))

;;
;; get-output-bytevector procedure
;;
(define (get-output-bytevector port)
  (scream:error:not-implemented "(get-output-bytevector)"))

(include 
  "i-ports-input.s"
  "i-ports-output.s")
