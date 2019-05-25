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

;;;; Command Argument Reader

;; Public
(define (with-command-argument-reader thunk)
  (fluid-let ((*magnitude* '())
	      (*negative?* '())
	      (*multiplier-exponent* '())
	      (*autoargument-mode?* '()))
    (thunk)))

;; Public
(define (reset-command-argument-reader!)
  ;; Call this at the beginning of a command cycle.
  (set-fluid! *magnitude* #!FALSE)
  (set-fluid! *negative?* #!FALSE)
  (set-fluid! *multiplier-exponent* 0)
  (set-fluid! *autoargument-mode?* #!FALSE))

;; Public
(define (command-argument-prompt)
  (let ((prefix (if (autoargument-mode?) "Autoarg" "Arg"))
	(value (command-argument-value)))
    (cond (value (string-append prefix " " (obj->string value)))
	  ((command-argument-negative?) (string-append prefix " -"))
	  (else ""))))

;; Public
(define (command-argument-negative?)
  (fluid *negative?*))

;; Public
(define (command-argument-value)
  ;; This returns the numeric value of the argument, or #!FALSE if none.
  (let ((m (command-argument-magnitude))
	(s (command-argument-multiplier-exponent)))
    (and (or m (not (zero? s)))
	 ((if (command-argument-negative?) - identity-procedure)
	  (* (or m 1)
	     (integer-expt (command-argument-multiplier-base) s))))))

;; Public
(define (command-argument-magnitude)
  (fluid *magnitude*))

;; Public
(define (command-argument-multiplier-exponent)
  (fluid *multiplier-exponent*))

;; Public
(define (command-argument-multiplier-base)
  *multiplier-base*)

;; Public
(define (autoargument-mode?)
  (fluid *autoargument-mode?*))

;;;; Value
(define integer-expt
  (lambda (b e)
     (if (zero? e)
	 1
	 (* b (integer-expt b (sub1 e))))))

