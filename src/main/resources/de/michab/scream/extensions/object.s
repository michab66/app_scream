;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 2001-2024 Michael G. Binz
;

#| utility-library candidate.
 |
 | SO: https://stackoverflow.com/questions/4261604/sorting-a-list-in-scheme
 |#
(define (qsort le? to-sort)
  (if
    (or (null? to-sort) (<= (length to-sort) 1)) to-sort
    (let loop (
      (left '())
      (right '())
      (pivot (car to-sort)) (rest (cdr to-sort)))
      (if (null? rest)
        (append (append (qsort le? left) (list pivot)) (qsort le? right))
             (if (le? (car rest) pivot)
                  (loop (append left (list (car rest))) right pivot (cdr rest))
                  (loop left (append right (list (car rest))) pivot (cdr rest)))))))

#| string-library candidate.
 |
 | Sorts the passed list of strings.
 |#
(define (sort-strings strings)
  (qsort string<=? strings))

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
 | Returns information on the constructors, methods and fields of the passed
 | class.  The resulting list is intended to be used with (assoc ...), do not
 | expect a particular order of the sublists.
 |
 | (describe-class class-name)
 |   => ((constructors "c1" "c2" ...)
 |       (methods "m1" "m2" ...)
 |       (fields "f1" "f2" ...))
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
        (vector->list (vector-map
          ctor->string 
          constructors)
        )
      )
      (cons 
        'methods
        (vector->list (vector-map
          method->string
          methods)
        )
      )
      (cons 
        'fields
        (vector->list (vector-map 
          field->string
          fields)
        )
      )
    )
  )
)

#|
 | Prints information on the constructors, methods and fields
 | of the passed class.
 |
 | (display-class java-class-name)
 |
 |#
(define (display-class class)
  (define (do-display what what-list)
    (scream:display-ln ";" what)
    (if (null? what-list)
      (scream:display-ln "None.")
      (for-each
        scream:display-ln
        (sort-strings what-list))))

  (let* 
    (
      (class-info
        (describe-class class))
      (constructors (cdr
        (assoc 'constructors class-info)))
      (methods (cdr
        (assoc 'methods class-info)))
      (fields (cdr
        (assoc 'fields class-info)))
    )

    (do-display "Constructors" constructors)
    (do-display "Methods" methods)
    (do-display "Fields" fields)

     scream:unspecified
  )
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
