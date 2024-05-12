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
(define (scream:assert func-name object predicate expected-type)
  (cond
    ((predicate object)
      object)
    (else
      (error 
        (scream:assert:error-arg func-name "TYPE_ERROR")
        expected-type
        (scream:typename object)
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
(define scream:type:input-port
  ((make-object "de.michab.scream.fcos.PortIn") "TYPE_NAME"))
(define scream:type:integer
  ((make-object "de.michab.scream.fcos.Int") "TYPE_NAME"))
(define scream:type:number
  ((make-object "de.michab.scream.fcos.Number") "TYPE_NAME"))
(define scream:type:output-port
  ((make-object "de.michab.scream.fcos.PortOut") "TYPE_NAME"))
(define scream:type:port
  ((make-object "de.michab.scream.fcos.Port") "TYPE_NAME"))
(define scream:type-procedure
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

(define scream:cons?
  (typePredicateGenerator "de.michab.scream.fcos.Cons" #t))

(define scream:integer?
  (typePredicateGenerator "de.michab.scream.fcos.Int" #t))

#|
 | Type assertions.
 |#
(define (scream:assert:boolean func-name value)
  (scream:assert func-name value scream:boolean? scream:type:boolean)
)

(define (scream:assert:bytevector func-name value)
  (scream:assert func-name value scream:bytevector? scream:type:bytevector)
)

(define (scream:assert:cons func-name value)
  (scream:assert func-name value scream:cons? scream:type:cons)
)

(define (scream:assert:char func-name value)
  (scream:assert func-name value character? scream:type:character)
)

(define (scream:assert:cons func-name value)
  (scream:assert func-name value pair? scream:type:cons)
)

(define (scream:assert:integer func-name value)
  (scream:assert func-name value scream:integer? scream:type:bytevector)
)

(define (scream:assert:list func-name value)
  (scream:assert func-name value list? "list")
)
(define (scream:assert:string func-name value)
  (scream:assert func-name value string? scream:type:string)
)
(define (scream:assert:symbol func-name value)
  (scream:assert func-name value symbol? scream:type:symbol)
)
(define (scream:assert:vector func-name value)
  (scream:assert func-name value vector? scream:type:vector)
)
