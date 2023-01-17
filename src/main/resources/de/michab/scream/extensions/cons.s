; $Id: cons.s 8 2008-09-14 14:23:20Z binzm $
;
; Scream / Runtime
;
; Comments contain text from R5RS
; Released under Gnu Public License
; Copyright (c) 1998-2000 Michael G. Binz



;;
;; (null? obj)      library procedure; r5rs 26
;;
(define (null? obj)
  (eqv? () obj))



;;
;; (pair? obj)         procedure; r5rs 26
;;
(define (pair? obj)
  (and
    (not (null? obj))
    ((typePredicateGenerator "de.michab.scream.fcos.Cons" #t) obj)))



;;
;; (cons obj1 obj2)    procedure; r5rs 26
;;
(define (cons obj1 obj2)
  (make-object (de.michab.scream.fcos.Cons obj1 obj2)))



;;
;; (set-car! pair new-car)       procedure; r5rs 26
;;
(define (set-car! pair new-car)
  (if (pair? pair)
    ((object pair) (setCar new-car))
    (error "TYPE_ERROR"
           %type-cons
           (%typename pair)
           1)))



;;
;; (set-cdr! pair new-cdr)       procedure; r5rs 26
;;
(define (set-cdr! pair new-cdr)
  (if (pair? pair)
    ((object pair) (setCdr new-cdr))
    (error "TYPE_ERROR"
           %type-cons
           (%typename pair)
           1)))



;;
;; (car pair)           procedure; r5rs 26
;; (cdr pair)           procedure; r5rs 26
;; (caar pair)
;; ...
;; (cddddr pair) library
;;
(define (car pair)
  (if (pair? pair)
    ((object pair) (getCar))
    (error "TYPE_ERROR"
           %type-cons
           (%typename pair))))

(define (cdr pair)
  (if (pair? pair)
    ((object pair) (getCdr))
    (error "TYPE_ERROR"
           %type-cons
           (%typename pair))))

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



;;
;; (list? obj) library procedure; r5rs 26
;;
(define (list? obj)
  (or
    (eqv? obj ())
    (and
      (pair? obj)
      ((object obj) (isProperList)))))



;;
;; (list obj ...) library procedure; r5rs 27
;;
(define (list . elements)
  elements)



;;
;; (length list) library procedure; r5rs 27
;;
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



;;
;; (reverse list) library procedure; r7rs 42
;;
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



;;
;; (append list ...) library procedure; r5rs 27
;;
(define (append list . other-lists)
  ;;
  ;; TODO: The body below is not needed.  This is an iteration expression.
  (do
    ;; Init
    ((lists-to-be-appended (reverse (cons list other-lists))))

    ;; Test
    ((= 1 (length lists-to-be-appended)) (car lists-to-be-appended))

    ;; Body
    (set! lists-to-be-appended
      (if (null? (cadr lists-to-be-appended))
        (cons
          (car lists-to-be-appended)
          (cddr lists-to-be-appended))
        (cons
          ((object (cadr lists-to-be-appended)) (append (car lists-to-be-appended)))
          (cddr lists-to-be-appended))))))



;;
;; (list-tail list k) library procedure; r5rs 27
;;
(define (list-tail list k)
  (if (pair? list)
    ((object list) (listTail k))
    (error "TYPE_ERROR"
           %type-cons
           (%typename list)
           1)))



;;
;; (list-ref list k) library procedure; r5rs 27
;;
(define (list-ref list k)
  (if (pair? list)
    ((object list) (listRef k))
    (error "TYPE_ERROR"
           %type-cons
           (%typename list)
           1)))




;;
;; (memq obj list) library procedure; r5rs 27
;;
(define (memq obj list)
  (if (list? list)
    ((object list) (memq obj))
    (error "TYPE_ERROR"
           %type-cons
           (%typename list)
           2)))



;;
;; (memv obj list) library procedure; r5rs 27
;;
(define (memv obj list)
  (if (list? list)
    ((object list) (memv obj))
    (error "TYPE_ERROR"
           %type-cons
           (%typename list)
           2)))



;;
;; (member obj list) library procedure; r5rs 27
;;
(define (member obj list)
  (if (list? list)
    ((object list) (member obj))
    (error "TYPE_ERROR"
           %type-cons
           (%typename list)
           2)))



;;
;; (assq obj alist) library procedure; r5rs 27
;; (assv obj alist) library procedure; r5rs 27
;; (assoc obj alist) library procedure; r5rs 27
;;
;; Alist (for "association list") must be a list of pairs. These procedures
;; find the first pair in alist whose car field is obj, and returns that pair.
;; If no pair in alist has obj as its car, then #f (not the empty list) is
;; returned. Assq uses eq? to compare obj with the car fields of the pairs in
;; alist, while assv uses eqv? and assoc uses equal?.
;;
(define (assq obj alist)
  (if (list? alist)
    ((object alist) (assq obj))
    (error "TYPE_ERROR"
           %type-cons
           (%typename alist)
           2)))

(define (assv obj alist)
  (if (list? alist)
    ((object alist) (assv obj))
    (error "TYPE_ERROR"
           %type-cons
           (%typename alist)
           2)))

(define (assoc obj alist)
  (if (list? alist)
    ((object alist) (assoc obj))
    (error "TYPE_ERROR"
           %type-cons
           (%typename alist)
           2)))
