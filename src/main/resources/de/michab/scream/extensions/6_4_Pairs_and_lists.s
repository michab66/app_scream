; Scream @ https://github.com/urschleim/scream
;
; Copyright © 1998-2024 Michael G. Binz

;;
;; r7rs 6.4 Pairs and lists, p40
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scream definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Init type name.
(define scream:type-cons
  ((make-object "de.michab.scream.fcos.Cons") "TYPE_NAME"))


#|
 | (scream:make-circular! list)
 |
 | Makes the passed list circular.
 | Returns the circular list.
 |#
(define (scream:make-circular! list)
    (set-cdr!
      ; of the last cons ...
      (list-tail list (- (length list) 1))
      ; ... to the begin of the list.
      list)
    list)

#|
 | (scream:circular? list1 ...)
 |
 | Returns #T if all of the the passed lists
 | are circular.
 |#
(define scream:circular?
  (scream:make-transitive
    (lambda (list)
      (cond
        ((null? list)
          #f)
        ((not (pair? list))
          (error "TYPE_ERROR" scream:type-cons list))
        (else
          ((object list) ("isCircular")))))))

#|
 | (scream:memx obj list compare)
 |
 | Implements the basic functionality for the memx... operations.
 |#
(define (scream:memx obj list compare)
  (cond
    ((not (procedure? compare))
      (error "TYPE_ERROR" scream:type-procedure compare))
    ((null? list)
      #f)
    ((compare obj (car list))
      list)
    (else
      (scream:memx obj (cdr list) compare))))

#|
 | (scream:assx obj alist compare)
 |
 | Implements the basic functionality for the assx... operations.
 |#
(define (scream:assx obj alist compare)
  (cond
    ((not (procedure? compare))
      (error "TYPE_ERROR" scream:type-procedure compare))
    ((null? alist)
      #f)
    ((compare obj (caar alist))
      (car alist))
    (else
      (scream:assx obj (cdr alist) compare))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; r7rs definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | (pair? obj) procedure p41
 |#
(define pair?
  (typePredicateGenerator "de.michab.scream.fcos.Cons" #t))

#|
 | (cons obj1 obj2) procedure p41
 |#
(define (cons obj1 obj2)
  (make-object ("de.michab.scream.fcos.Cons:de.michab.scream.fcos.FirstClassObject,de.michab.scream.fcos.FirstClassObject" obj1 obj2)))

#|
 | (car pair) procedure p41
 |#
(define (car pair)
  (if (pair? pair)
    ((object pair) (getCar))
    (error "TYPE_ERROR"
           %type-cons
           (scream:typename pair))))

#|
 | (cdr pair) procedure p41
 |#
(define (cdr pair)
  (if (pair? pair)
    ((object pair) (getCdr))
    (error "TYPE_ERROR"
           %type-cons
           (scream:typename pair))))

#|
 | (set-car! pair new-car) procedure p41
 |#
(define (set-car! pair new-car)
  (if (pair? pair)
    ((object pair) (setCar new-car))
    (error "TYPE_ERROR"
           %type-cons
           (scream:typename pair)
           1)))

#|
 | (set-cdr! pair new-cdr) procedure p41
 |#
(define (set-cdr! pair new-cdr)
  (if (pair? pair)
    ((object pair) (setCdr new-cdr))
    (error "TYPE_ERROR"
           %type-cons
           (scream:typename pair)
           1)))

#|
 | (caar pair) p42
 | ...
 | (cddddr pair) library procedures
 |#
(define (caar pair) (car (car pair)))
(define (cadr pair) (car (cdr pair)))
(define (cdar pair) (cdr (car pair)))
(define (cddr pair) (cdr (cdr pair)))

(define (caaar pair) (car (car (car pair))))
(define (caadr pair) (car (car (cdr pair))))
(define (cadar pair) (car (cdr (car pair))))
(define (caddr pair) (car (cdr (cdr pair))))
(define (cdaar pair) (cdr (car (car pair))))
(define (cdadr pair) (cdr (car (cdr pair))))
(define (cddar pair) (cdr (cdr (car pair))))
(define (cdddr pair) (cdr (cdr (cdr pair))))

(define (caaaar pair) (car (car (car (car pair)))))
(define (caaadr pair) (car (car (car (cdr pair)))))
(define (caadar pair) (car (car (cdr (car pair)))))
(define (caaddr pair) (car (car (cdr (cdr pair)))))
(define (cadaar pair) (car (cdr (car (car pair)))))
(define (cadadr pair) (car (cdr (car (cdr pair)))))
(define (caddar pair) (car (cdr (cdr (car pair)))))
(define (cadddr pair) (car (cdr (cdr (cdr pair)))))
(define (cdaaar pair) (cdr (car (car (car pair)))))
(define (cdaadr pair) (cdr (car (car (cdr pair)))))
(define (cdadar pair) (cdr (car (cdr (car pair)))))
(define (cdaddr pair) (cdr (car (cdr (cdr pair)))))
(define (cddaar pair) (cdr (cdr (car (car pair)))))
(define (cddadr pair) (cdr (cdr (car (cdr pair)))))
(define (cdddar pair) (cdr (cdr (cdr (car pair)))))
(define (cddddr pair) (cdr (cdr (cdr (cdr pair)))))

#|
 | (null? obj) procedure p42
 |#
(define (null? obj)
  (eqv? '() obj))

#|
 | (list? obj) procedure p42
 |#
(define (list? obj)
  (or
    (eqv? obj ())
    (and
      (pair? obj)
      ((object obj) (isProperList)))))

#|
 | (make-list k) procedure; r7rs 6.4 p42
 | (make-list k fill) procedure; r7rs 6.4 p42
 |#
; => cons-delayed.s

#|
 | (list obj ...) procedure; r7rs 6.4 p42
 |#
(define (list . elements)
  elements)

#|
 | (length list) procedure; r7rs 6.4 p42
 |#
(define (length list)
  (cond
    ;; If list argument is nil...
    ((eqv? list ())
      ;; ...then length is zero.
      0)
    ;; If this is a non-null list...
    ((list? list)
      ;; ...ask for its length.
      ((object list) (length)))
    (else
      (error "EXPECTED_PROPER_LIST"))))

#|
 | (append list ...) procedure; r7rs 6.4 p42
 |#
(define (append . list)
  ; Remove a leading empty list element from the arguments.
  ; This strategy is from Chez-Scheme.
  (if (and (not (null? list)) (null? (car list)))
    (set! list (cdr list)))
    
  (cond
    ; No argument -> empty list.
    ((null? list) '())
    ; One argument -> return unmodified.
    ((eq? 1 (length list)) (car list))
    (else
      (begin
        ; Ensure that o is a pair.
        (define (assert-cons o position)
          (if (pair? o)
            o
            (error "TYPE_ERROR"
              %type-cons
              (scream:typename o)
              position)))
        ; Implement the actual append.
        (define (append-impl position list)
          (let ((current (car list)) (rest (cdr list)))
            (if (null? rest)
              current
              ; Calls fco.Cons#append
              ((object (assert-cons current position))
                (append (append-impl (+ position 1) rest)))
            ) ; if
          ) ; let
        ) ; define
        (append-impl 1 list)
      ) ; begin
    ) ; else
  ) ; cond
) ; define

#|
 | (reverse list) procedure; r7rs 6.4 p42
 |#
(define (reverse list)
  (cond
    ((equal? list ()) 
      ())
    ((list? list)
      ((object list) (reverse)))
    (else
      (error "EXPECTED_PROPER_LIST"))
  )
)

#|
 | (list-tail list k) procedure; r7rs p42
 |#
(define (list-tail list k)
  (if (pair? list)
    ((object list) (listTail k))
    (error "TYPE_ERROR"
           %type-cons
           (scream:typename list)
           1)))

#|
 | (list-ref list k) procedure; r7rs 6.4 p42
 |#
(define (list-ref list k)
  (if (pair? list)
    ((object list) (listRef k))
    (error "TYPE_ERROR"
           %type-cons
           (scream:typename list)
           1)))
#|
 | (list-set! list k obj) procedure r7rs 6.4 p43
 |#
(define (list-set! list k obj)
  (if (< k 0)
    (error "INDEX_OUT_OF_BOUNDS" k))
  (if (not (list? list))
    (error "EXPECTED_PROPER_LIST" list))

  (define (_list-set! list i)
    (cond
      ((null? list)
        (error "INDEX_OUT_OF_BOUNDS" k))
      ((= i 0)
	      (set-car! list obj))
      (else
        (_list-set! (cdr list) (- i 1)))))

  (_list-set! list k))

#|
 | (memq obj list) procedure; r7rs p43
 |#
(define (memq obj list)
  (scream:memx obj list eq?))

#|
 | (memv obj list)  procedure; r7rs p43
 |#
(define (memv obj list)
  (scream:memx obj list eqv?))

#|
 | (member obj list) procedure; r7rs 6.4 p43
 | (member obj list compare) procedure; r7rs 6.4 p43
 |#
; => cons-delayed.s

#|
 | (assq obj alist) procedure; r7rs p43
 |#
(define (assq obj alist)
  (scream:assx obj alist eq?))

#|
 | (assv obj alist) procedure; r7rs p43
 |#
(define (assv obj alist)
  (scream:assx obj alist eqv?))

#|
 | (assoc obj alist) procedure; r7rs 6.4 p43
 | (assoc obj alist compare) procedure; r7rs 6.4 p43
 |#
; => cons-delayed.s

#|
 | (list-copy obj) procedure r7rs 6.4 p43
 |#
(define (list-copy list)
  (if (null? list)
      '()
      (cons (car list)
            (list-copy (cdr list)))))
