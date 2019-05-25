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

;;; This code isn't packaged yet.  Public definitions are marked.

;;;; Description
;;;
;;; 1.  The reader keeps track of:
;;;
;;; [] The MAGNITUDE of the argument.  If there are no digits, the
;;;    magnitude is #!FALSE.
;;; [] The SIGN of the argument.
;;; [] The MULTIPLIER-EXPONENT, which is the number of C-U's typed.
;;; [] Whether or not "Autoargument mode" is in effect.  In autoarg
;;;    mode, ordinary digits are interpreted as part of the argument;
;;;    normally they are self-insering.
;;;
;;; 2.  It has the following (alterable) parameters:
;;;
;;; [] RADIX, which is between 2 and 36 inclusive. (default: 10)
;;; [] MULTIPLIER-BASE, a non-negative integer. (default: 4)
;;;
;;; 3.  From these, it can compute:
;;;
;;; [] VALUE = (* MAGNITUDE MULTIPLIER-EXPONENT MULTIPLIER-BASE).
;;;    If the magnitude is #!FALSE, then the value is too.

;;;; Primitives

;; Public
;(define (with-command-argument-reader thunk)

;; Public
;(define (reset-command-argument-reader!)

;; Public
(define (update-argument-prompt!)
  (set-command-prompt! (command-argument-prompt)))

;; Public
;(define (command-argument-prompt)

;;;; Argument Number

(define *radix*)

;; Public
(define (command-argument-accumulate-digit! digit-char)
  (maybe-reset-multiplier-exponent!)
  (let ((digit (or (char->digit digit-char *radix*)
		   (error "Not a valid digit" digit-char))))
    (set-fluid! *magnitude*
	  (if (not (fluid *magnitude*))
	      digit
	      (+ digit (* *radix* (fluid *magnitude*)))))))

;; Public
(define (set-command-argument-radix! n)
  (if (not (and (integer? n) (<= 2 n) (<= n 36)))
      (error "Radix must be an integer between 2 and 36, inclusive" n))
  (set! *radix* n))

;; Public
(define (command-argument-negate!)
  (maybe-reset-multiplier-exponent!)
  (set-fluid! *negative?* (not (fluid *negative?*))))

;; Public
;(define (command-argument-magnitude)

;; Public
(define (command-argument-radix)
  *radix*)

;; Public
;(define (command-argument-negative?)

;; **** Kludge ****
(set-command-argument-radix! 10)

;;;; Argument Multiplier


(define *multiplier-base*)

;; Public
(define (command-argument-increment-multiplier-exponent!)
  (set-fluid! *multiplier-exponent* (1+ (fluid *multiplier-exponent*))))

(define (maybe-reset-multiplier-exponent!)
  (if (and (not (fluid *magnitude*))
	   (= (fluid *multiplier-exponent*) 1))
      (set-fluid! *multiplier-exponent* 0)))

;; Public
;(define (command-argument-multiplier-exponent)

;; Public
;(define (command-argument-multiplier-base)

;; Public
(define (set-command-argument-multiplier-base! n)
  (if (not (and (integer? n) (not (negative? n))))
      (error "Multiplier Base" n "must be a non-negative integer."))
  (set! *multiplier-base* n))

;; **** Kludge ****
(set-command-argument-multiplier-base! 4)

;;;; Autoargument Mode

;; Public
(define (enter-autoargument-mode!)
  (set-fluid! *autoargument-mode?* #!TRUE))

;; *** Is this needed? ***
;;(define (exit-autoargument-mode!)
;;  (set-fluid! *autoargument-mode?* #!FALSE))

;; Public
;(define (autoargument-mode?)


;;;; Value
;(define integer-expt

;; Public
;(define (command-argument-value)

;; Public
(define (command-argument-multiplier-only?)
  (and (not (fluid *magnitude*))
       (not (zero? (fluid *multiplier-exponent*)))
       (fluid *multiplier-exponent*)))

;; Public
(define (command-argument-negative-only?)
  (and (not (fluid *magnitude*))
       (zero? (fluid *multiplier-exponent*))
       (fluid *negative?*)))

;; Public
(define (command-argument-beginning?)
  (and (not (fluid *magnitude*))
       (not (fluid *negative?*))
       (< (fluid *multiplier-exponent*) 2)))

(define (%edwin-autoargument argument)
  (let ((char (char-base (current-command-char))))
    (if (eq? char #\-)
     	(if (command-argument-beginning?)
	    (begin (enter-autoargument-mode!)
		   (^r-negative-argument-command argument))
	    (insert-chars char argument (current-point)))
	(begin (enter-autoargument-mode!)
	       (^r-argument-digit-command argument)))))

