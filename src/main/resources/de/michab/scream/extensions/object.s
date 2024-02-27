;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 2001-2024 Michael G. Binz
;

#|
 |
 |#
#;(define (get-class string)
  ((make-object2 "java.lang.Class") (forName string)))

#|
 |
 |#
(%syntax (make-object-2 ctor-spec)
   ;(scream:display-ln 'list? (list? ctor-spec) ctor-spec)
   ;(scream:display-ln 'element (car ctor-spec)(cadr ctor-spec))

  ; Evaluate the elements in ctor-spec.
  ; TODO copy ctor spec.
  ; Wait for procedure (eval exp) github#322 
  
  ;(scream:display-ln 'ctor-spec ctor-spec)

  (cond
    ((scream:string? ctor-spec)
      (scream:java:make-class ctor-spec))
    ((list? ctor-spec)
    (let ((copy (list-copy ctor-spec)))
    (do
      (
        (current copy (cdr current))
      )
      ((null? current) (apply scream:java:make-instance copy))
     
      (set-car! current (scream:eval (car current)))
    )
    )
      ;(apply scream:java:make-instance ctor-spec)
      )
    (else
      (error "TYPE_ERROR" 'stringOrCons ctor-spec)))
)

#|
 |
 |#
(define (display-object class)
  (define exp `(let* 
    (
      (class-info (make-object-2 ,class))
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
      (class-adapter (make-object de.michab.scream.binding.JavaClassAdapter))
      (proxy (class-adapter (createObject (list->vector class-list))))
    )
    (proxy (instanciateInterface))))

#|
 |
 |#
(define (find-method class-obj method-name . argument-class-list)
  (class-obj (getMethod method-name (list->vector argument-class-list))))
