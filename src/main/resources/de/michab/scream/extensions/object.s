;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 2001-2024 Michael G. Binz
;

#|
 |
 |#
#;(define (get-class string)
  ((make-object2 "java.lang.Class") ("forName:java.lang.String" string)))

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
