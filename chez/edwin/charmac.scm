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

;;; Character Sets macros

(macro make-char-set
  (lambda (e)
    (list->string  (cdr e))))

(define (%loop code chars predicate)
  (if (< code 256)
      (let ((char (integer->char code)))
        (%loop (1+ code)
              (if (predicate char)
                  (cons char chars)
                  chars)
               predicate))
       chars))

(macro predicate->char-set
  (lambda (e)
     (list->string (%loop 0 '() (eval (cadr e))))))


(macro char-set-invert
  (lambda (e)
    (list->string (%loop 0 '()
                         (lambda (char)
                           (not (char-set-member? (eval (cadr e)) char)))))))

(macro char-set-union
  (lambda (e)
    (list->string (%loop 0 '()
      (lambda (char)
        (or (char-set-member? (eval (cadr e)) char)
            (char-set-member? (eval (caddr e)) char)))))))

(macro make-non-graphic-char-set
  (lambda (e)
    (let ((set (make-string 32 (integer->char 128))))
      (do ((i 0 (1+ i)))
	  ((= i 32) set)
	(string-set! set i (integer->char i))))))

