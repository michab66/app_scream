;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 1998-2023 Michael G. Binz
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scream specific
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (scream:exec:textual:input operation port)
  (let
    (
      (port
        (cond
          ((null? port)
            (current-input-port))
          ((= 1 (length port))
            (scream:assert-type (car port) input-port? scream:type-input-port) )
          (else
            (scream:error:wrong-number-of-arguments 1 (length port)))))
      (operation
        (scream:assert-type operation symbol? scream:type-symbol))
    )
    (if (not (textual-port? port))
      (error "EXPECTED_TEXTUAL_PORT" port)
      (scream:eval
        (quasiquote ((object port) ((unquote operation))))
      )
    ) ; if
  ) ; let
)

(define (scream:exec:with:textual:input::port operation port)
  (let
    (
      (port
        (cond
          ((null? port)
            (current-input-port))
          ((= 1 (length port))
            (scream:assert-type (car port) input-port? scream:type-input-port) )
          (else
            (scream:error:wrong-number-of-arguments 1 (length port)))))
      (operation
        (scream:assert-type operation procedure? scream:type-procedure))
    )
    (if (not (textual-port? port))
      (error "EXPECTED_TEXTUAL_PORT" port)
      (operation port)
    ) ; if
  ) ; let
)

(define (scream:exec:with:binary:input::port operation port)
  (let
    (
      (port
        (cond
          ((null? port)
            (current-input-port))
          ((= 1 (length port))
            (scream:assert-type (car port) input-port? scream:type-input-port) )
          (else
            (scream:error:wrong-number-of-arguments 1 (length port)))))
      (operation
        (scream:assert-type operation procedure? scream:type-procedure))
    )
    (if (not (binary-port? port))
      (error "EXPECTED_BINARY_PORT" port)
      (operation port)
    ) ; if
  ) ; let
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; r7rs 6.13.2 Input p57
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read library procedure
;;
(define (read . port)
  (scream:exec:textual:input 'read port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read-char procedure
;;
(define (read-char . port)
  (scream:exec:textual:input 'readCharacter port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; peek-char procedure
;;
(define (peek-char . port)
  (scream:exec:textual:input 'peekCharacter port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read-line procedure
;;
(define (read-line . port)
  (scream:exec:with:textual:input::port 
    (lambda (port) 
      ((object port) (readLine)))
    port))
;(define (read-line . port)
;  (scream:exec:textual:input 'readLine port))	  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eof-object? procedure
;;
(define (eof-object? obj)
  (eq? (eof-object) obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eof-object procedure
;;
(define eof-object
  (let
    (
      (cached ((make-object de.michab.scream.fcos.Port) EOF))
    )
    (lambda () cached)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; char-ready? procedure
;;
(define (char-ready? . port)
  (scream:exec:textual:input 'charReady port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read-string procedure
;;
(define (read-string k . port)
  (scream:exec:with:textual:input::port 
    (lambda (port) 
      ((object port) (readString k)))
    port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read-u8 procedure
;;
(define (read-u8 port)
  (scream:error:not-implemented "(read-u8)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; peek-u8 procedure
;;
(define (peek-u8 port)
  (scream:error:not-implemented "(read-u8)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; u8-ready? procedure
;;
(define (u8-ready? . port)
  (scream:error:not-implemented "(u8-ready?)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read-bytevector procedure
;; #109
(define (read-bytevector k port)
  (scream:error:not-implemented "(read-bytevector)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read-bytevector! procedure
;; #109
(define (read-bytevector! bytevector port start end)
  (scream:error:not-implemented "(read-bytevector!)"))
