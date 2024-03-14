;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 1998-2024 Michael G. Binz
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scream specific
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define scream:type-symbol
  ((make-object "de.michab.scream.fcos.Symbol") "TYPE_NAME"))

;;
;; (symbol? symbol) procedure; r5rs 30
;;
(define symbol?
  (typePredicateGenerator "de.michab.scream.fcos.Symbol" #t))



;;
;; Returns the name of symbol as a string. If the symbol was part of an object
;; returned as the value of a literal expression (section 4.1.2) or by a call
;; to the read procedure, and its name contains alphabetic characters, then the
;; string returned will contain characters in the implementation's preferred
;; standard case -- some implementations will prefer upper case, others lower
;; case. If the symbol was returned by string->symbol, the case of characters
;; in the string returned will be the same as the case in the string that was
;; passed to string->symbol. It is an error to apply mutation procedures like
;; string-set! to strings returned by this procedure.
;;
(define (symbol->string symbol)
  (if (symbol? symbol)
    ((object symbol) (toString))
    (error "TYPE_ERROR" %type-symbol (scream:typename symbol))))


;;
;; Returns the symbol whose name is string. This procedure can create symbols
;; with names containing special characters or letters in the non-standard
;; case, but it is usually bad idea to create such symbols because in some
;; implementations of Scheme they cannot be read as themselves.
;;
(define (string->symbol string)
  (if (string? string)
    ((make-object "de.michab.scream.fcos.Symbol") (createObject string))
    (error "TYPE_ERROR" %type-string (scream:typename symbol))))
