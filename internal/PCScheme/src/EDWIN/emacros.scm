;;;
;;;	Copyright (c) 1985 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;     Modified by Texas Instruments Inc 8/15/85
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;

(begin
  (define-integrable substring-find-next-char
                     substring-find-next-char-in-set)
  (define-integrable substring-find-previous-char
                     substring-find-previous-char-in-set)
)
;;;; Replace Group

(define (string-replace string char1 char2)
  (let ((string (string-copy string)))
    (string-replace! string char1 char2)
    string))

(define (substring-replace string start end char1 char2)
  (let ((string (string-copy string)))
    (substring-replace! string start end char1 char2)
    string))

(define (string-replace! string char1 char2)
  (substring-replace! string 0 (string-length string) char1 char2))

(define (substring-replace! string start end char1 char2)
  (define (loop start)
    (let ((index (substring-find-next-char string start end char1)))
      (if index
	  (sequence (string-set! string index char2)
		    (loop (1+ index))))))
  (loop start))

(define string-uppercase '())
(let ()
  (define (string-set-case char-set-case)
    (lambda (string1)
      (let ((end (string-length string1)))
        (define (loop string2 string1 index char-set-case end)
          (if (= index end)
              string2
              (begin (string-set! string2
                                  index
                                  (char-set-case (string-ref string1 index)))
                     (loop string2 string1 (1+ index) char-set-case end))))
        (loop (make-string end '()) string1 0 char-set-case end))))
  (set! string-uppercase (string-set-case char-upcase)))

(define map2
  (lambda (fn arg1 arg2)
    (cond ((or (null? arg1) (null? arg2)) '())
          (t (cons (fn (car arg1) (car arg2))
                   (map2 fn (cdr arg1) (cdr arg2)))))))

(macro define-named-structure
  (lambda (e)
    (let ((name (cadr e)) (slots (cddr e)))
         (define ((make-symbols x) y) (make-symbol x y))
         (define (make-symbol . args)
                 (string->symbol (apply string-append args)))
	 (let ((structure-string (string-uppercase name))
               (slot-strings (mapcar symbol->string slots)))
              (let ((prefix (string-append structure-string "-")))
                   (let ((structure-name (string->symbol structure-string))
                         (tag-name (make-symbol "%" prefix "TAG"))
                         (constructor-name
                          (make-symbol "%MAKE-" structure-string))
                         (predicate-name (make-symbol structure-string "?"))
                         (slot-names
                          (mapcar (make-symbols
                                   (string-append prefix "INDEX:"))
                                  slot-strings))
                         (selector-names
                          (mapcar (make-symbols prefix) slot-strings)))
                        (define (slot-loop tail slot-names n)
                                (if (null? slot-names)
                                    tail
                                    (slot-loop (cons (list 'DEFINE-INTEGRABLE
                                                           (car
                                                            slot-names)
                                                           n)
                                                     tail)
                                               (cdr slot-names)
                                               (|1+| n))))
                        (define (selector-loop tail selector-names n)
                                (if (null? selector-names)
                                    tail
                                    (selector-loop
                                      (cons `(define-integrable
                                               ,(car selector-names)
                                                 (lambda (,structure-name)
                                                  (vector-ref ,structure-name
                                                              ,n)))
                                             tail)
                                       (cdr selector-names)
                                       (|1+| n))))
                        `(begin
                           (define ,tag-name ,name)
                           (define (,constructor-name)
                             (let ((,structure-name
                                    (make-vector ,(1+ (length slots)) '())))
                               (vector-set! ,structure-name 0 ,tag-name)
                               ,structure-name))
;;;                           (define (,predicate-name object)
;;;                             (and (vector? object)
;;;                                  (not (zero? (vector-size object)))
;;;                                  (eq? ,tag-name (vector-ref object 0))))
                           ,@(slot-loop '() slot-names 1)
                           ,@(selector-loop '() selector-names 1))))))))

(macro define-command
  (lambda (e)
    (let ((bvl (cadr e)) (description (caddr e)) (body (cdddr e)))
         (let ((name (car bvl))
               (arg-names
                (mapcar (lambda (arg)
                          (if (pair? arg) (car arg) arg))
                        (cdr bvl)))
               (arg-inits
                (mapcar (lambda (arg)
                          (if (pair? arg) (cadr arg) #!FALSE))
                        (cdr bvl))))
              (let ((procedure-name
                     (string->symbol
                      (string-append (canonicalize-name-string name)
                                     "-COMMAND"))))
                   `(begin
                      (define (,procedure-name ,@arg-names)
                        ,@(map2 (lambda (arg-name arg-init)
                                  `(if (not ,arg-name)
                                       (set! ,arg-name ,arg-init)))
                                arg-names arg-inits)
                        ,@body)
                      (make-command ,name ,description ,procedure-name)))))))

(define canonicalize-name-string
  (lambda (name)
    (let ((name (string-uppercase name)))
	 (string-replace! name #\Space #\-)
         name)))



