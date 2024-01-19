; Scream @ https://github.com/urschleim/scream
;
; Copyright © 1998-2023 Michael G. Binz

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scream definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Procedure type name.
(define scream:type-procedure
  ((make-object de.michab.scream.fcos.Procedure) TYPE_NAME))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; r7rs definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | (procedure? obj) essential procedure; r5rs 27
 |#
(define procedure?
  (typePredicateGenerator "de.michab.scream.fcos.Procedure" #f))

#|
 | (apply proc arg1 ... args)  procedure; r7rs 50
 |#
(define (apply op . list)
  (define (make-argument-list list)
    (let ((first (car list)) (rest (cdr list)))
      (if (null? rest)
        first
        (cons first (make-argument-list rest)))))
  (scream:apply op (make-argument-list list)))

#|
 | (map proc list1 list2 ... ) r7rs 6.10 p51 procedure
 |#
(define (map procedure . lists)
  ;(display "_map:lists= ") (display lists)(newline)

  (define (p-simple-car lists)
    (if (null? lists)
      '()
      (cons (caar lists) (p-simple-car (cdr lists)))))
      
  (define (p-simple-cdr lists)
    (if (null? lists)
      '()
      (cons (cdar lists) (p-simple-cdr (cdr lists)))))

  (define (__map  lists)
;   (display "__map: ") (display lists) (newline)
    (cond
      ((null? lists)
        '())
      ((memv '() lists)
        '())
      (else
        (let
          (
            (slice (p-simple-car lists))
            (roast (p-simple-cdr lists))
          )
;          (display "slice: ") (display slice) (newline)
          (cons (apply procedure slice) (__map roast))))
    )
  )

  (cond
    ((not (procedure? procedure))
      (error "TYPE_ERROR" scream:type-procedure procedure))
    ((circular? lists)
      (error "ILLEGAL_ARGUMENT" "Only circular lists"))
    (else
      (__map  lists))))

#|
 | (string-map proc string1 string2 ... ) r7rs 6.10 p51 procedure
 |#
(define (string-map proc . strings)
  (scream:display-ln 'string-map strings)
 
  ; r7rs string-copy is currently not implemented.
  ; Switch as soon this is available.
  (define (_string-copy string start)
    (substring string start (string-length string)))

  (define (has-content? string)
    (< 0 (string-length string)))

  (define have-content?
    (scream:make-transitive has-content?))

  ; Accepts a list of strings, returns a list containing
  ; the first characters of the strings.
  (define (strings-first strings)
    (scream:display-ln "strings-first" strings)
    (scream:transform
      (lambda (string) (string-ref string 0))
      strings))

  ; Accepts a list of strings, returns a list containing
  ; the strings with the first characters removed.
  (define (strings-rest strings)
    (scream:display-ln "strings-rest" strings)
    (scream:transform
      (lambda (string) (_string-copy string 1))
      strings))

  ; maps proc on the passed strings, returns a list of characters.
  (define (_string-map result strings)
    (scream:display-ln '_string-map result strings)
    (if (apply have-content? strings)
        (_string-map
          (cons (apply proc (strings-first strings)) result)
          (strings-rest strings))
        (apply string (reverse result))))

  (cond
    ((procedure? proc)
      (_string-map '() strings))
    (else
      (error "TYPE_ERROR" scream:type-procedure proc))))

#|
 | (vector-map proc vector₁ string2 ... ) r7rs 6.10 p51 procedure
 |#

;;
;; (for-each proc list1 list2 ... ) library procedure
;;
(define for-each map)

#|
 | (values obj ...) r7rs 6.10 p53 procedure
 |#
(define (values . things)
     (call-with-current-continuation
        (lambda (cont) (apply cont things))))

#|
 | (call-with-values producer consumer) r7rs 6.10 p53 procedure
 |#
;; Implemented in Continuation.java

;;
;; See https://github.com/urschleim/scream/issues/221
;;
(define (eq? x y)
    (%fco-class (eq x y)))

(define (eqv? x y)
    (%fco-class (eqv x y)))

(define (equal? x y)
    (%fco-class (equal x y)))
