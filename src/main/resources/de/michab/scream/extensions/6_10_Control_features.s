; Scream @ https://github.com/urschleim/scream
;
; Copyright © 1998-2024 Michael G. Binz

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scream definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Procedure type name.
(define scream:type-procedure
  ((make-object de.michab.scream.fcos.Procedure) TYPE_NAME))

;
; Returns true if none of the passed lists is null.
; (scream:valid-list-slice? list1 ...)
;
(define scream:valid-list-slice?
  (scream:make-transitive 
    (lambda (list)
      (not (null? list)))))

;
; Splices a list as used by map and for-each into a slice
; containing all cars and into a rest containing the remaining
; cdrs of the passed lists.
; This is returned as two values.
;
(define (scream:splice . lists)
;    (scream:display-ln 'scream:splice lists)

    (define (_splice result-car result-cdr lists)
;      (scream:display-ln '_splice lists)
      (if (null? lists)

        (values
          (reverse result-car)
          (reverse result-cdr))

        (_splice
          (cons (caar lists) result-car)
          (cons (cdar lists) result-cdr)
          (cdr lists))))

    (_splice '() '() lists))

;
; Returns an engine-expression returning slices from the lists.  If no
; further slice can be returned returns the empty list.
;
(define (scream:list-slicer . lists)
  (let ((lists (list-copy lists)))
;    (scream:display-ln 'scream:list-slicer lists)

    (lambda ()
;      (scream:display-ln 'scream:list-slicer lists)
      
      (let (
             (valid? (apply scream:valid-list-slice? lists))
           )
;        (scream:display-ln 'scream:list-slicer 'valid? valid?)
       
        (if (not valid?)
          '()
          (let-values (((cars rests) (apply scream:splice lists)))
;            (scream:display-ln 'cars cars)
;            (scream:display-ln 'rests rests)
            (set! lists rests)
            cars))))))

;
; Returns true if the passed index k is a valid index in the
; passed string.
;
(define (scream:valid-string-index? k string)
;  (scream:display-ln 'scream:valid-string-index k string)
  
  (and 
    (>= k 0)
    (< k (string-length string))))

;
; Returns true if k is a valid index in all strings.
;
(define (scream:valid-strings-index? k strings)
;    (scream:display-ln 'scream:valid-strings-index k strings)
  (if (null? strings)
    #t
    (if (scream:valid-string-index? k (car strings))
      (scream:valid-strings-index? k (cdr strings))
      #f)))

;
; Returns a cons-slice of the values at position k of
; the passed strings.
;
(define (scream:slice-strings k strings)
;    (scream:display-ln 'scream:slice-strings k strings)

  (define (_slice-strings result strings)
    (if (null? strings)
      (reverse result)
      (_slice-strings
        (cons (string-ref (car strings) k) result)
        (cdr strings))))

  (_slice-strings '() strings))

;
; Returns an engine-expression returning slices at index 0 1 ...
; from the passed strings.  If no further slice can be returned
; returns the empty list.
;
(define (scream:string-slicer . strings)
  (let ((current-position -1))
;    (scream:display-ln 'make-string-slicer current-position strings)

    (lambda ()
      (set! current-position (+ 1 current-position))

;      (scream:display-ln 'scream:string-slicer current-position strings)

      (let (
             (valid? (scream:valid-strings-index? current-position strings))
           )
;        (scream:display-ln 'scream:string-slicer 'valid? valid?)

        ; is current position after the end of a string?
         (if (not valid?)
        ;  yes: return the empty list
          '()
        ;  no: collect all elements at this position and return this as the result.
          (scream:slice-strings current-position strings))))))

;
; Returns true if the passed index k is a valid index in the
; passed vector.
;
(define (scream:valid-vector-index? k vector)
;    (scream:display-ln 'scream:valid-vector-index k vector)
  
    (and 
      (>= k 0)
      (< k (vector-length vector))))

;
; Returns true if k is a valid index in all vectors.
;
(define (scream:valid-vectors-index? k vectors)
;  (scream:display-ln 'scream:valid-vectors-index k vectors)
  (if (null? vectors)
    #t
    (if (scream:valid-vector-index? k (car vectors))
      (scream:valid-vectors-index? k (cdr vectors))
      #f)))

;
; Returns a cons-slice of the values at position k of
; the passed vectors.
;
(define (scream:slice-vectors k vectors)
;    (scream:display-ln 'scream:slice-vectors k vectors)

  (define (_slice-vectors result vectors)
    (if (null? vectors)
      (reverse result)
      (_slice-vectors
        (cons (vector-ref (car vectors) k) result)
        (cdr vectors))))

  (_slice-vectors '() vectors))

#|
 | Returns an engine-expression returning slices at index 0 1 ...
 | from the passed vectors.  If no further slice can be returned
 | returns the empty list.
 |#
(define (scream:vector-slicer . vectors)
  (let ((current-position -1))
;    (scream:display-ln 'scream:make-vector-slicer current-position vectors)

    (lambda ()
      (set! current-position (+ 1 current-position))
      
;      (scream:display-ln 'scream:vector-slicer current-position vectors)

      (let (
             (valid? (scream:valid-vectors-index? current-position vectors))
           )
;        (scream:display-ln 'scream:vector-slicer 'valid? valid?)

        ; is current position after the end of a vector?
        (if (not valid?)
        ;  yes: return the empty list
          '()
        ;  no: collect all elements at this position and return this as the result.
          (scream:slice-vectors current-position vectors))))))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; r7rs definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|
 | (procedure? obj) essential procedure; r5rs 27
 |#
(define procedure?
  (typePredicateGenerator "de.michab.scream.fcos.Procedure" #f))

#|
 | (apply proc arg₁ ... args)  procedure; r7rs 50
 |#
(define (apply op . list)
  (define (make-argument-list list)
    (let ((first (car list)) (rest (cdr list)))
      (if (null? rest)
        first
        (cons first (make-argument-list rest)))))
  (scream:apply op (make-argument-list list)))

#|
 | (map proc list₁ list₂ ... ) r7rs 6.10 p51 procedure
 |#
(define (map proc . lists)
;  (scream:display-ln 'map lists)

  (let ((slicer (apply scream:list-slicer lists)))

    (define (_map result)
      (let ((current (slicer)))
        (if (null? current)
          (reverse result)
          (_map 
            (cons (apply proc current) result)))))

    (if (apply scream:circular? lists)
      (error "ILLEGAL_ARGUMENT" 'all-circular)
      (_map '()))))

#|
 | (string-map proc string₁ string₂ ... ) r7rs 6.10 p51 procedure
 |#
(define (string-map proc . strings)
;  (scream:display-ln 'string-map strings)
  
  (let ((slicer (apply scream:string-slicer strings)))
  
    (define (_string-map result)
      (let ((current (slicer)))
        (if (null? current)
          (apply string (reverse result))
          (_string-map 
            (cons (apply proc current) result)))))

    (_string-map '())))

#|
 | (vector-map proc vector₁ vector₂ ...) r7rs 6.10 p51 procedure
 |#
(define (vector-map proc . strings)
;  (scream:display-ln 'vector-map strings)
  
  (let ((slicer (apply scream:vector-slicer strings)))
  
    (define (_vector-map result)
      (let ((current (slicer)))
        (if (null? current)
          (apply vector (reverse result))
          (_vector-map 
            (cons (apply proc current) result)))))

    (_vector-map '())))

#|
 | (for-each proc list₁ list₂ ...)) r7rs 6.10 p51 procedure
 |#
(define (for-each proc . lists)
;  (scream:display-ln 'for-each lists)

  (let ((slicer (apply scream:list-slicer lists)))

    (define (_for-each)
      (let ((current (slicer)))
        (if (null? current)
          scream:unspecified
          (begin
            (apply proc current)
            (_for-each)))))

    (_for-each)))

#|
 | (string-for-each proc string₁ string₂ ... ) r7rs 6.10 p52 procedure
 |#
(define (string-for-each proc . strings)
;  (scream:display-ln 'string-for-each strings)
  
  (let ((slicer (apply scream:string-slicer strings)))

    (define (_string-for-each)
      (let ((current (slicer)))
        (if (null? current)
          scream:unspecified
          (begin
           (apply proc current)
           (_string-for-each)))))

    (_string-for-each)))

#|
 | (vector-for-each proc vector₁ vector₂ ...) r7rs 6.10 p52 procedure
 |#
(define (vector-for-each proc . vectors)
;  (scream:display-ln 'vector-for-each vectors)

  (let ((slicer (apply scream:vector-slicer vectors)))

    (define (_vector-for-each)
      (let ((current (slicer)))
        (if (null? current)
          scream:unspecified
          (begin
            (apply proc current)
            (_vector-for-each)))))
            
    (_vector-for-each)))

#|
 | (call-with-current-continuation proc) r7rs 6.10 p52 procedure
 |#
;; Implemented in Continuation.java

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

#|
 | (dynamic-wind before thunk after) r7rs 6.10 p53 procedure
 |#
(define (dynamic-wind before thunk after)
  (error "NOT_IMPLEMENTED" 'dynamic-wind))
