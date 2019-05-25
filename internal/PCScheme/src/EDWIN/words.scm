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

;;;; Words

(define (forward-word mark n limit?)
  (cond ((positive? n) (%forward-word mark n limit?))
	((negative? n) (%backward-word mark (- n) limit?))
	(else mark)))

(define (%forward-word mark n limit?)
  (let ((end (group-end mark)))
    (define (loop mark n)
      (let ((m (find-next-word-constituent mark end #!FALSE)))
	(if (not m)
	    (limit-mark-motion limit? mark)
	    (let ((m (find-next-word-delimiter m end 'LIMIT)))
	      (if (= n 1)
		  m
		  (loop m (-1+ n)))))))
    (loop mark n)))

(define (backward-word mark n limit?)
  (cond ((positive? n) (%backward-word mark n limit?))
	((negative? n) (%forward-word mark (- n) limit?))
	(else mark)))

(define (%backward-word mark n limit?)
  (let ((end (group-start mark)))
    (define (loop mark n)
      (let ((m (find-previous-word-constituent mark end #!FALSE)))
	(if (not m)
	    (limit-mark-motion limit? mark)
	    (let ((m (find-previous-word-delimiter m end 'LIMIT)))
	      (if (= n 1)
		  m
		  (loop m (-1+ n)))))))
    (loop mark n)))

(define (forward-to-word mark limit?)
  (find-next-word-constituent mark (mark-end mark) limit?))

(define (find-next-word-constituent start end limit?)
  (or (find-next-char-in-set start end word-constituent-chars)
      (limit-mark-motion limit? end)))

(define (find-previous-word-constituent start end limit?)
  (or (find-previous-char-in-set start end word-constituent-chars)
      (limit-mark-motion limit? end)))

(define (find-next-word-delimiter start end limit?)
  (or (find-next-char-in-set start end word-delimiter-chars)
      (limit-mark-motion limit? end)))

(define (find-previous-word-delimiter start end limit?)
  (or (find-previous-char-in-set start end word-delimiter-chars)
      (limit-mark-motion limit? end)))
