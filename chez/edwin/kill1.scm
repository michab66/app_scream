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

;;;; Kill Commands

(define append-next-kill-tag "Append Next Kill")

(define (delete-region mark)
  (if (not mark) (editor-error "Delete exceeds buffer bounds"))
  (region-delete! (make-region (current-point) mark)))

(define (kill-region mark)
  (if (not mark) (editor-error "Kill exceeds buffer bounds"))
  (let* ((point (current-point))
         (forward? (mark<= point mark)))
    (%kill-region (region-extract! (make-region point mark)) forward?)))

(define (%kill-region region forward?)
  (let ((ring (current-kill-ring)))
    (command-message-receive append-next-kill-tag
      (lambda ()
	(if (ring-empty? ring) (editor-error "No previous kill"))
	(region-insert! ((if forward? region-end region-start)
			 (ring-ref ring 0))
			region))
      (lambda ()
	(ring-push! ring region))))
  (set-command-message! append-next-kill-tag))

(define (un-kill-region region)
  (set-current-region! (region-insert (current-point) region)))

