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

(define (copy-region mark)
  (if (not mark) (editor-error "Copy exceeds buffer bounds"))
  (let ((point (current-point)))
    (%kill-region (region-copy (make-region point mark))
		  (mark<= point mark))))

(define (un-kill-region-reversed region)
  (set-current-region-reversed! (region-insert (current-point) region)))

(define (%edwin-un-kill-pop argument)
  (command-message-receive un-kill-tag
    (lambda ()
      (region-delete! (make-region (current-point) (pop-current-mark!)))
      (let ((ring (current-kill-ring)))
	;; **** Missing test for equality here.
	(if (not (zero? argument))
	    (begin (ring-pop! ring)
		   (un-kill-region-reversed (ring-ref ring 0))))))
    (lambda ()
      (editor-error "No previous un-kill to replace")))
  (set-command-message! un-kill-tag))

