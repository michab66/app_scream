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

;;;  equating language feature

(alias vector-cons make-vector)

(alias vector-size vector-length)

(alias conjunction and)

(alias disjunction or)

(macro string-allocate
   (lambda (e)
     (list 'make-string (cadr e) " ")))


;;; Following equations are temporary till I understand them better

(macro without-interrupts
  (lambda (e)
    (cdr e)))


(macro define-unparser
  (lambda (e)
    '()))

(macro declare (lambda (e) '()))

(macro integrate (lambda (e) '()))

(macro primitive-datum
   (lambda (e) (cadr e)))

(macro set!
  (lambda (e)
    (if (= 2 (length e))
        (list (car e) (cadr e) '#!unassigned)
        e)))

;;; some to remove name clashes

;;; line-length is a pcs primitive. we are changing it to line-string-length
;;; by defining a macro.

(define-integrable line-string-length
  (lambda (line)
     (string-length (line-string line))))

(macro line-length
   (lambda (e)
      (cons 'line-string-length (cdr e))))



