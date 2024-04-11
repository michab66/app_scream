;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 2001-2024 Michael G. Binz
;

#| string-library candidate.
 |
 | Joins the strings delimited by the delimiter.
 |
 | (string-join delimiter . strings)
 |
 | (string-join "-" "michel" "binz" "loves" "many" "things")
 |  => "michel-binz-loves-many-things"
 |#
(define (string-join delimiter . strings)
  (define (_string-join result string-list)
    (if (null? string-list)
      result
      (_string-join
        (string-append result delimiter (car string-list))
        (cdr string-list))))

  (let
    (
      (result 
        (_string-join "" strings))
    )
    ; Remove the leading delimiter.
    (substring result 1 (string-length result))
  )
)

#|
 |
 |#
(define (describe-class class-name)

  (define (named-parameter-list name parameter-types)
        (string-append
          name
          ":"
          (apply string-join
            ","
            (vector->list
              (vector-map
                (lambda (c) (c ("getCanonicalName")))
                parameter-types
              ) ; vector-map
            ) ; vector->list
          ) ; apply
        ) ; string-append
  )
  
  #|
   | Expects a java.reflect.Constructor.  Creates a Scream constructor
   | description in the form
   |  "<typename>[:arg-type1[,...]]".
   |
   | Examples:
   |  "java.lang.String" ; Default ctor.
   |  "java.lang.String:byte[],java.nio.charset.Charset"
   |#
  (define (ctor->string ctor)
    (let*
      (
        (name
          ((ctor ("getDeclaringClass")) ("getCanonicalName")))
        (parameter-types
          (ctor ("getParameterTypes")))
      )
      (if (= 0 (vector-length parameter-types))
        name
        (named-parameter-list name parameter-types)
      ) ; if
    ) ; let*
  )

  (define (method->string meth)
    (let*
      (
        (name
          (meth ("getName")))
        (parameter-types
          (meth ("getParameterTypes")))
      )
      (if (= 0 (vector-length parameter-types))
        name
        (named-parameter-list name parameter-types)
      ) ; if
    ) ; let*
  )
  
  (define (field->string field)
    (field ("getName")))

  (let*
    (
      (class (scream:java:make-class-object class-name))
      (constructors (class ("getConstructors")))
      (methods (class ("getMethods")))
      (fields (class ("getFields")))
    )

    (list
      (cons 
        'constructors
        (vector-map
          ctor->string 
          constructors
        )
      )
      (cons 
        'methods
        (vector-map
          method->string
          methods
        )
      )
      (cons 
        'fields
        (vector-map 
          field->string
          fields)
      )
    )
  )
)

#|
 |
 |#
(define (display-object class)
  (define exp `(let* 
    (
      (class-info (make-object ,class))
      (info (describe-object class-info))      
    )
    (scream:display-ln "Operations:")
    (vector-for-each
      (lambda (i) (scream:display-ln i))
      (car info))
    (scream:display-ln "Members:")
    (vector-for-each
      (lambda (i) (scream:display-ln i))
      (cdr info))

     scream:unspecified
  ))

  (scream:eval exp)
)

#|
 |
 |#
#;(define (make-interface . rest)
  (if (null? rest)
    (error "NOT_ENOUGH_ARGUMENTS" 1))
  (let*
    (
      (class-list (map get-class rest))
      (class-adapter (make-object "de.michab.scream.binding.JavaClassAdapter"))
      (proxy (class-adapter (createObject (list->vector class-list))))
    )
    (proxy (instanciateInterface))))

#|
 |
 |#
(define (find-method class-obj method-name . argument-class-list)
  (class-obj (getMethod method-name (list->vector argument-class-list))))
