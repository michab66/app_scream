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

;;; sum global definitions


(define integer-divide
   (lambda (a b)
      (cons (quotient a b) (remainder a b))))

(define integer-divide-quotient car)

(define integer-divide-remainder cdr)

(define char->name
  (lambda (char)
    (define (%char->name char)
      (let ((i (char->integer char)))
        (cond ((zero? i) "")
              ((= i 27) "Meta-")
              ((and (>= i 1) (<= i 31))
               (string-append "Ctrl-" (char->name (integer->char (+ i 64)))))
              (t (list->string (list char))))))
    (if (atom? char)
        (%char->name char)
        (string-append (%char->name (car char))
                       (%char->name (cadr char))))))
(define string-append-separated
  (lambda (s1 s2)
    (cond ((zero? (string-length s1)) s2)
          ((zero? (string-length s2)) s1)
          (else (string-append s1 " " s2)))))

(define string-append-with-blanks
  (lambda strings
    ((rec loop
       (lambda (strings)
         (if (null? strings) ""
             (string-append-separated (car strings) (loop (cdr strings))))))
     strings)))

(define char->string
  (lambda (char)
    (if (char? char)
        (char->name char)
	(error "Bad argument to char->string" char))))

(define list->string*
  (lambda (l)
    (if (pair? l)
	(string-append "("
			   (apply string-append-with-blanks
				  (mapcar obj->string l))
		       ")")
	(error "Bad argument to list->string*" l))))

(define obj->string
  (lambda (obj)
    (cond ((pair? obj) (list->string* obj))
	  ((char? obj) (char->string obj))
	  ((integer? obj) (number->string obj '(INT)))
	  ((null? obj) "()")
	  (t (error "Bad argument to obj->string" obj)))))

(define char-base char->integer)

(define char->digit
  (lambda (i radix)
    (- i (char->integer #\0))))

(define identity-procedure (lambda (x) x))





