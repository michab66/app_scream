;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 1998-2024 Michael G. Binz
;

;;
;; r7rs 6.13.1 p56 
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scream definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; r7rs definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; call-with-port procedure
;;
(define (call-with-port port proc)
  (let* 
    (
      (port
        (scream:assert:port 'call-with-port port))
      (proc
        (scream:assert:procedure 'call-with-port proc))
      (result (proc port))
    )

    (close-port port)
    result
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; call-with-input-file file library procedure
;;
(define (call-with-input-file string proc)
  (scream:assert:string 'call-with-input-file string)
  (scream:assert:procedure 'call-with-input-file proc)

  (call-with-port
    (open-input-file string)
    proc)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; call-with-output-file file library procedure
;;
(define (call-with-output-file string proc)
  (scream:assert:string 'call-with-output-file string)
  (scream:assert:procedure 'call-with-output-file proc)

  (call-with-port
    (open-output-file string)
    proc)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; input-port? procedure
;;
(define (input-port? port)
  (or
    (scream:input-port? port) 
    (scream:binary-input-port? port)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; output-port? procedure
;;
(define (output-port? port)
  (or
    (scream:output-port? port)
    (scream:binary-output-port? port)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; binary-port? procedure
;;
(define (binary-port? port)
  (if (port? port)
    ((object port) ("isBinary"))
    #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; textual-port? procedure
;;
(define (textual-port? port)
  (and (port? port) (not (binary-port? port))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; port? procedure
;;
(define port?
  scream:port?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; input-port-open? procedure
;;
(define (input-port-open? port)
  (if (input-port? port)
    (not ((object port) ("isClosed")))
    #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; output-port-open? procedure
;;
(define (output-port-open? port)
  (if (output-port? port)
    (not ((object port) ("isClosed")))
    #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; current-input-port procedure
;;
;; (scream:current-input-port-push port)
;; (scream:current-input-port-pop)
;;
(define current-input-port ())
(define scream:current-input-port-push ())
(define scream:current-input-port-pop ())

(let ((current-input-port-stack '()))
  (set! current-input-port
    (lambda ()
      (if (null? current-input-port-stack)
        (scream:evaluator ("getInPort"))
        (car current-input-port-stack))))

  (set! scream:current-input-port-push
    (lambda (port)

      (scream:assert-type 
        port
        input-port?
        scream:type:port)

      (set! current-input-port-stack (cons port current-input-port-stack))))

  (set! scream:current-input-port-pop
    (lambda ()
      (if (not (null? current-input-port-stack))
       (set! current-input-port-stack (cdr current-input-port-stack)))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; current-output-port procedure
;;
;; (scream:current-output-port-push port)
;; (scream:current-output-port-pop)
;;
(define current-output-port ())
(define scream:current-output-port-push ())
(define scream:current-output-port-pop ())

(let ((current-output-port-stack '()))
  (set! current-output-port
    (lambda ()
      (if (null? current-output-port-stack)
        (scream:evaluator ("getOutPort"))
        (car current-output-port-stack))))

  (set! scream:current-output-port-push
    (lambda (port)

      (scream:assert-type 
        port
        output-port?
        scream:type:port)

      (set! current-output-port-stack (cons port current-output-port-stack))))

  (set! scream:current-output-port-pop
    (lambda ()
      (if (not (null? current-output-port-stack))
       (set! current-output-port-stack (cdr current-output-port-stack)))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; current-error-port procedure
;;
;; Note that the value is not cacheable since the
;; port may be set individually for each invocation.
(define (current-error-port)
  (scream:evaluator ("getErrorPort")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; with-input-from-file file library procedure
;;
(define (with-input-from-file string thunk)
  (scream:current-input-port-push
    (open-input-file string))
  (let ((result (thunk)))
    (scream:current-input-port-pop)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; with-output-to-file file library procedure
;;
(define (with-output-to-file string thunk)
  (scream:current-output-port-push
    (open-output-file string))
  (let ((result (thunk)))
    (scream:current-output-port-pop)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; open-input-file file library procedure
;;
(define (open-input-file string)
  (make-object ("de.michab.scream.fcos.PortIn:java.lang.String" string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; open-binary-input-file file library procedure
;; TODO not tested.
(define (open-binary-input-file string)
  (make-object ("de.michab.scream.fcos.PortInBinary:java.lang.String" string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; open-output-file file library procedure
;;
(define (open-output-file string)
  (make-object ("de.michab.scream.fcos.PortOut:java.lang.String" string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; open-binary-output-file file library procedure
;;
(define (open-binary-output-file filename)
   (make-object (de.michab.scream.fcos.PortOutBinary string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; close-port  procedure
;;
(define (close-port port)
  ((object (scream:assert:port 'close-port port)) ("close"))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; close-input-port procedure
;;
(define (close-input-port port)
  (close-port (scream:assert:input-port 'close-input-port port))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; close-output-port  procedure
;;
(define (close-output-port port)
  (close-port (scream:assert:output-port 'close-output-port port))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; open-input-string procedure
;;
(define (open-input-string string)
  (let
    ((reader (make-object ("java.io.StringReader:java.lang.String" string))))
    (make-object ("de.michab.scream.fcos.PortIn:java.lang.String,java.io.Reader" "scream:input-string" reader))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; open-output-string procedure
;;
(define (open-output-string)
  (let
    ((writer (make-object ("java.io.StringWriter"))))
    (make-object ("de.michab.scream.fcos.PortOut:java.lang.String,java.io.Writer" "scream:output-string" writer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-output-string procedure
;;
(define (get-output-string port)
  (let* (
    (stream ((object port) ("stream")))
    (port-class (stream ("getClass")))
    (writer-class (make-object "java.io.StringWriter"))
    )
    (if (equal? port-class writer-class)
      (stream ("toString"))
      'EOF)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; open-input-bytevector procedure
;;
(define (open-input-bytevector bytevector)
  (scream:assert:bytevector 'open-input-bytevector bytevector)

  (make-object 
    ("de.michab.scream.fcos.PortInBinary:java.lang.String,java.io.InputStream"
      "scream:input-bytevector" 
      ((object bytevector) ("asStream"))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; open-output-bytevector procedure
;;
(define (open-output-bytevector)
  (make-object
    ("de.michab.scream.fcos.PortOutBinary:java.lang.String,java.io.OutputStream"
      "scream:output-bytevector"
      (make-object
        ("java.io.ByteArrayOutputStream")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-output-bytevector procedure
;;
(define (get-output-bytevector port)
  (scream:assert:binary-output-port 'get-output-bytevector port)

  ; TODO check name of port.  Must be "scream:output-bytevector".
  
  (let* (
    (stream ((object port) ("stream")))
    (array ((object stream) ("toByteArray"))) )
    
    (make-object ("de.michab.scream.fcos.Bytevector:byte[]" array))
  )
)
