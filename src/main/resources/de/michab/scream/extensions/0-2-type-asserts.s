;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 2024 Michael G. Binz
;

#|
 | (make-name-type func-name error-name)
 |
 | Expects a symbol 'func-name and a string 'error-name.
 | Creates a string "func-name:error-name" to be used as the first
 | argument of the (error ...) procedure.
 |#
(define (scream:assert:error-arg symbol-name string-error)
  (let*
    (
      (function
        (scream:class:fco ("forWrite:de.michab.scream.fcos.FirstClassObject" symbol-name)))
      (function-colon
        ((object function) ("append:de.michab.scream.fcos.SchemeString" ":")))
    )
    
    ((object function-colon) ("append:de.michab.scream.fcos.SchemeString" string-error))
  )
)

#|
 | (scream:assert-type object predicate expected-type)
 |
 | Asserts that the passed predicate evaluates to true for the passed
 | object and returns the passed object.  If the predicate does not
 | evaluate to true, then a type_error is thrown.
 |#
(define (scream:assert func-name object predicate expected-type . position)
  (cond
    ((predicate object)
      object)
    ((scream:null? position)
      (error 
        (scream:assert:error-arg func-name "TYPE_ERROR")
        expected-type
        (scream:typename object)
      )
    )
    (else
      (error 
        (scream:assert:error-arg func-name "TYPE_ERROR")
        expected-type
        (scream:typename object)
        (car position)
      )
    )
  )
)

#|
 | Define the type names.
 |#
(define scream:type:boolean
  ((make-object "de.michab.scream.fcos.Bool") "TYPE_NAME"))
(define scream:type:bytevector
  ((make-object "de.michab.scream.fcos.Bytevector") "TYPE_NAME"))
(define scream:type:char
  ((make-object "de.michab.scream.fcos.SchemeCharacter") "TYPE_NAME"))
(define scream:type:cons
  ((make-object "de.michab.scream.fcos.Cons") "TYPE_NAME"))
(define scream:type:integer
  ((make-object "de.michab.scream.fcos.Int") "TYPE_NAME"))
(define scream:type:number
  ((make-object "de.michab.scream.fcos.Number") "TYPE_NAME"))
(define scream:type:port
  ((make-object "de.michab.scream.fcos.Port") "TYPE_NAME"))
(define scream:type:input-port
  ((make-object "de.michab.scream.fcos.PortIn") "TYPE_NAME"))
(define scream:type:binary-input-port
  ((make-object "de.michab.scream.fcos.PortInBinary") "TYPE_NAME"))
(define scream:type:output-port
  ((make-object "de.michab.scream.fcos.PortOut") "TYPE_NAME"))
(define scream:type:binary-output-port
  ((make-object "de.michab.scream.fcos.PortOutBinary") "TYPE_NAME"))
(define scream:type:procedure
  ((make-object "de.michab.scream.fcos.Procedure") "TYPE_NAME"))
(define %scream:type:real
  ((make-object "de.michab.scream.fcos.Real") "TYPE_NAME"))
(define scream:type:string
  ((make-object "de.michab.scream.fcos.SchemeString") "TYPE_NAME"))
(define scream:type:symbol
  ((make-object "de.michab.scream.fcos.Symbol") "TYPE_NAME"))
(define scream:type:vector
  ((make-object "de.michab.scream.fcos.Vector") "TYPE_NAME"))

#|
 | Low-level type predicate definitions.
 |#
(define scream:boolean?
  (typePredicateGenerator "de.michab.scream.fcos.Bool" #t))

(define scream:bytevector?
  (typePredicateGenerator "de.michab.scream.fcos.Bytevector" #t))

(define scream:char?
  (typePredicateGenerator "de.michab.scream.fcos.SchemeCharacter" #t))

(define scream:cons?
  (typePredicateGenerator "de.michab.scream.fcos.Cons" #t))

(define scream:integer?
  (typePredicateGenerator "de.michab.scream.fcos.Int" #t))

(define scream:port?
  (typePredicateGenerator "de.michab.scream.fcos.Port" #f))
(define scream:input-port?
  (typePredicateGenerator "de.michab.scream.fcos.PortIn" #t))
(define scream:binary-input-port?
  (typePredicateGenerator "de.michab.scream.fcos.PortInBinary" #t))
(define scream:output-port?
  (typePredicateGenerator "de.michab.scream.fcos.PortOut" #t))
(define scream:binary-output-port?
  (typePredicateGenerator "de.michab.scream.fcos.PortOutBinary" #t))

(define scream:procedure?
  (typePredicateGenerator "de.michab.scream.fcos.Procedure" #f))
(define scream:string?
  (typePredicateGenerator "de.michab.scream.fcos.SchemeString" #t))

#|
 | Type assertions.
 |#
(define (scream:assert:boolean func-name value)
  (scream:assert func-name value scream:boolean? scream:type:boolean)
)

(define (scream:assert:bytevector func-name value)
  (scream:assert func-name value scream:bytevector? scream:type:bytevector)
)

(define (scream:assert:char func-name value)
  (scream:assert func-name value scream:char? scream:type:char)
)

(define (scream:assert:cons func-name value)
  (scream:assert func-name value scream:cons? scream:type:cons)
)

(define (scream:assert:char func-name value)
  (scream:assert func-name value char? scream:type:char)
)

(define (scream:assert:cons func-name value)
  (scream:assert func-name value scream:cons? scream:type:cons)
)

(define (scream:assert:integer func-name value)
  (scream:assert func-name value scream:integer? scream:type:bytevector)
)

(define (scream:assert:list func-name value)
  (scream:assert func-name value scream:list? "list")
)

(define (scream:assert:port func-name value)
  (scream:assert func-name value scream:port? scream:type:port)
)
(define (scream:assert:input-port func-name value)
  (scream:assert func-name value scream:input-port? scream:type:input-port)
)
(define (scream:assert:binary-input-port func-name value)
  (scream:assert func-name value scream:binary-input-port? scream:type:binary-input-port)
)
(define (scream:assert:output-port func-name value)
  (scream:assert func-name value scream:output-port? scream:type:output-port)
)
(define (scream:assert:binary-output-port func-name value)
  (scream:assert func-name value scream:binary-output-port? scream:type:binary-output-port)
)

(define (scream:assert:procedure func-name value)
  (scream:assert func-name value scream:procedure? scream:type:procedure)
)

(define (scream:assert:string func-name value . position)
  (scream:assert func-name value string? scream:type:string position)
)
(define (scream:assert:symbol func-name value)
  (scream:assert func-name value symbol? scream:type:symbol)
)
(define (scream:assert:vector func-name value)
  (scream:assert func-name value vector? scream:type:vector)
)
