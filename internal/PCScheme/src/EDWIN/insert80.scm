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
;;;	Modified by Texas Instruments Inc 8/15/85
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; These routines are implemented to get speed in character
;;;; insertion and deletions. They are written so as not to effect
;;;; the code too much.
;;;; Changes arise in the command for delete character and delete backward
;;;; character in allcoms.scm. Also, redisplay contains an statement for the
;;;; creation of the daemons.
;;;; These should be faster and should create less garbage.

(define region-insert-char!
  (lambda (mark char)
    (if (eq? char #\newline)
	(region-insert-newline! mark)
	(begin
	  (if (group-read-only? (mark-group mark))
	      (editor-error "Trying to modify read only text"))
	  ((lambda (line pos)
	     (%region-insert-char! line pos char))
	   (mark-line mark) (mark-position mark))
	  (%recompute-for-insert-del-char!  mark)))))

(define %region-insert-char!
  (letrec
    ((%receiver
	(lambda (mark cursor?)
	  (if (or (> (mark-position mark) %pos)
		  (and (= (mark-position mark) %pos)
		       (mark-left-inserting? mark)))
	      (set-mark-position! mark (1+ (mark-position mark))))))
     (%pos '()))

    (lambda (line pos char)
      (set! %pos pos)
      (for-each-mark! line %receiver)
      (line-insert-char! line pos char))))


(define %recompute-for-insert-del-char! '())

(define %create-char-daemon
  (lambda (window)
    (set! %recompute-for-insert-del-char!
	  (%char-daemon window))))

(define (%char-daemon window)
  (lambda (mark)
    (let ((buffer (vector-ref window window:buffer))
	  (table  (vector-ref window window:lines))
	  (line   (mark-line mark))
	  (y (line->y window (mark-line mark)))
	  (y-size (vector-ref window window:y-size)))
      (let ((inferior (vector-ref table y)))
	(let ((old-ys (inferior:y-size inferior))
	      (new-ys (find-y-size line)))
	  (buffer-modified! buffer)
	  (if (= old-ys new-ys)
	      (begin
	       (maybe-marks-changed window y)
	       (set-start-end! window y y)
	       (cursor-moved! window))
	      (begin
	       (if (< old-ys new-ys)
		   (scroll-lines-down! window (- new-ys old-ys) y-size
                           table (+ (inferior:y-start inferior) old-ys))
		   (scroll-lines-up! window (- old-ys new-ys) y-size
			  table (+ (inferior:y-start inferior) old-ys)))
	       (set-inferior:y-size! inferior new-ys)
	       (fill-entries (1+ y) 
			     (+ (inferior:y-start inferior) new-ys)
			     y table y-size)
	       (set-start-end! window y (-1+ y-size))
	       (everything-changed! window window-redraw!))))))))


(define %region-delete-char!
  (letrec
    ((%receiver
	(lambda (mark cursor?)
	  (cond ((> (mark-position mark) end-pos)
		 (set-mark-position! mark (- (mark-position mark)
					     offset)))
		((> (mark-position mark) %pos)
		 (set-mark-position! mark %pos)))))
     (%pos '())
     (end-pos '())
     (offset 1))

  (lambda (mark)
    (letrec
      ((%%region-delete-char!
	 (lambda (line pos)
	   (set! %pos pos)
	   (set! end-pos (1+ pos))
	   (for-each-mark! line %receiver)
	   (subline-extract! line pos (1+ pos)))))

    (if (group-read-only? (mark-group mark))
	(editor-error "Trying to modify read only text"))
    (%%region-delete-char! (mark-line mark) (mark-position mark))
    (%recompute-for-insert-del-char! mark)))))





