;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 1998-2024 Michael G. Binz
;

#|
 | (scream:typename obj)
 |
 | Get the typename of the passed object as a string.
 | The typename of '() is "NIL".
 |#
(define (scream:typename x)
  (scream:class:fco ("getTypename:de.michab.scream.fcos.FirstClassObject" x)))
 
;;
;; (scream:assert-type object predicate expected-type)
;;
;; Asserts that the passed predicate evaluates to true for the passed
;; object and returns the passed object.  If the predicate does not
;; evaluate to true, then a type_error is thrown.
;;
(define (scream:assert-type object predicate expected-type)
  (if (predicate object)
    object
    (error "TYPE_ERROR" expected-type (scream:typename object))))

;;
;; (scream:to-transitive proc2) => (proc2 a b ...)
;;
;; Takes a procedure proc2 accepting two arguments.
;; Returns a procedure that accepts a variable number of
;; arguments calling proc2 as follows:
;;
;; (proc2 (proc2 (proc2 1 2) 3) 4)
;;
;; That is, the result is a transitive version of proc2.
;;

(define (scream:to-transitive proc2)
  (define (transitiver a b . rest)
         (if (scream:null? rest)
           (proc2 a b)
           (apply transitiver (proc2 a b) rest)))
    transitiver)

#|
 | (scream:display-ln  ...)
 |
 | Displays the passed arguments space delimited on stdout
 | and prints a newline after the last argument.
 |# 
(define (scream:display-ln . rest)
  (if (scream:null? rest)
    (newline)
    (begin
      (display (car rest))
      (display #\space)
      (apply scream:display-ln (cdr rest))))
)

(define (scream:gc)
  (scream:java:lang:system (gc)))

;;
;;
;; @proc typePredicateGenerator
;;
;; Is used for generating most of the type predicate procedures like vector?,
;; char?, etc. in the system.
;;
;; @arg string-type-name : string
;;   The java type name that has to be tested against.
;;
;; @arg exact-match : boolean
;;   if #T the generated predicate checks for an
;;   exact type match.  If #F it is allowed that
;;   the object passed to the resulting predicate
;;   is assignable to an element of the type
;;   passed to this procedure.
;;
;; @returns A procedure that has the standard type predicate signature,
;; accepting any single argument and returning a boolean.
;;
(define (typePredicateGenerator string-type-name exact-match)
  (let ((classObject ((make-object "java.lang.Class") ("forName:java.lang.String" string-type-name))))
    (lambda (obj)
      (cond
        ;; If this is NIL false is returned.
        ((scream:null? obj) #f)
        ;; Exact match test.
        (exact-match
          (classObject ("equals:java.lang.Object" ((object obj) ("getClass")))))
        ;; Assignable match test.
        (else
          (classObject ("isAssignableFrom:java.lang.Class" ((object obj) ("getClass")))))))))

;;
;; (transitiveBoolean proc)
;;
;; Accepts a procedure (proc arg1 arg2) that evaluates to a boolean.  Returns
;; a procedure that implements (proc arg1 arg2 ...).  The passed procedure
;; has to be transitive.
;;
(define (transitiveBoolean proc)
  (lambda (first second . rest)

    (do ((args
           ; Init - we just create a homogenous list of our arguments.  We have our
           ; arguments defined as they are to get automatic parameter checking for
           ; the at least required two params.
           (append (list first second) rest)
           ; Step - just remove the first entry from the arg list.
           (cdr args))
         ; Our break flag.
         (result #T))

        ; The test expression.
        ((or
            ; Either the remaining list length is less than two...
            (< (length args) 2)
            ; ..or the last evaluation of our procedure was false.
            (not result))
         ; We return our result.
         result)

        ; For each loop iteration we call our passed procedure with the first
        ; two entries of the remaining argument list.
        (set! result (proc (car args) (cadr args))))))

#|
 | Takes a single argument predicate and creates a predicate that
 | allows n arguments.
 | (define numbers? (scream:make-transitive number?)
 | (numbers? 1 2 3 4 5) => #t
 | (numbers?) => #f
 | (numbers? 1 'a 3 4 5) => #f
 |#
(define (scream:make-transitive proc)
  (define (_make-transitive . rest)
    ; (display rest)(newline)
    (if (scream:null? rest)
      #t
      (if (proc (car rest))
        (apply _make-transitive (cdr rest))
        #f)))

  (lambda list 
    ; (display list)(newline)
    ; Returns #f if called with an empty list.
    (if (scream:null? list)
      #f
      (apply _make-transitive list))))
