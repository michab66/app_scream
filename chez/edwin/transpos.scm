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

(define (twiddle-characters m1 m2)
  (let ((m* (mark-left-inserting m2)))
    (region-insert! m* (region-extract! (make-region (mark-1+ m1 'ERROR) m1)))
    (set-current-point! m*)))

(define (%edwin-transpose-characters argument)
  (cond ((conjunction (= argument 1) (line-end? (current-point)))
	 (twiddle-characters (mark-1+ (current-point) 'ERROR)
			     (current-point)))
	((positive? argument)
	 (twiddle-characters (current-point)
			     (mark+ (current-point) argument 'ERROR)))
	((negative? argument)
	 (twiddle-characters (current-point)
			     (mark- (current-point) (1+ (- argument)) 'ERROR)))
	(else
	 (let ((m1 (mark-right-inserting (current-point)))
	       (m2 (mark-right-inserting (current-mark))))
	   (let ((r1 (region-extract!
		      (make-region (current-point)
				   (mark1+ (current-point) 'ERROR))))
		 (r2 (region-extract!
		      (make-region (current-mark)
				   (mark1+ (current-mark) 'ERROR)))))
	     (region-insert! m1 r2)
	     (region-insert! m2 r1))
	   (set-current-point! m1)
	   (set-current-mark! m2)))))



