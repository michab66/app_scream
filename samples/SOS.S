;;;
;;; A simple prototype/delegation based object system
;;;
;;; Copyright 1995 Brent Benson (bwb@acm.org)
;;;

; Examples;
;
; (define obj1 (make-object 'foo (lambda (self) 4)))
; (obj1 'foo) -> 4
; (define obj2 (make-object 'parent (lambda (self) obj1)))
; (obj2 'foo) -> 4
; (obj2 'set-method 'bar (lambda (self arg) (+ arg (self 'foo))))
; (obj2 'bar 5) -> 9

; (make-object 'name method ...)
;
(define (make-object . methods)
  (let ((table (make-table)))
    (let loop ((methods methods))
      (cond
        ((null? methods)
         (create-object table))
        ((null? (cdr methods))
         (error "no matching method" (car methods)))
        (else
          (let ((message (car methods))
                (method (cadr methods)))
            (table-set! table message method)
            (loop (cddr methods))))))))

; Return a closure representing the object
;
(define (create-object method-table)

  ;; OBJECT is the object who will provide the message.
  ;; RECEIVER is the object to whom the method is applied.
  ;;
  (define (apply-method object receiver message . args)
    (let ((method (table-ref method-table message)))
      (if method
          (apply method (append (list receiver) args)) ; Micha
          (let ((parent-method (table-ref method-table 'parent)))
            (if parent-method
                (apply (parent-method object)
                       (append (list
                         'apply-method
                         receiver
                         message)
                               args))
                (error "bad message" message receiver))))))

;; This is the procedure that is the actual object.
;;
(define (self message . args)
  (case message
    ;; first check for special messages

    ;; In this case, we have been delegated the message from a child.
    ((apply-method)
     (apply apply-method (append (list self
                                       (car args)
                                       (cadr args))
                                 (cddr args))))

    ;; Set the method if present, otherwise add a new method.
    ((set-method)
     (let ((name (car args))
           (method (cadr args)))
       (table-set! method-table name method)
       name))

    ;; Normal case
    (else (apply apply-method (append (list self self message) args)))))

  self)


; table

(define (make-table)
  (vector '()))

(define (table-set! table key val)
  (let ((pair (assq key (vector-ref table 0))))
    (if pair
        (set-cdr! pair val)
        (vector-set! table 0 (cons (cons key val)
                                   (vector-ref table 0))))))

(define (table-ref table key)
  (let ((pair (assq key (vector-ref table 0))))
    (if pair
        (cdr pair)
        #f)))



;; This is Michas test.  We not only need methods, but also
;; local state!

; Create a new and empty object...
(define named (make-object))
; ...and add some state.
(let ((name ">> not set <<"))
  (named 'set-method 'get-name (lambda (self) name))
  (named 'set-method 'set-name (lambda (self nuname) (set! name nuname))))

