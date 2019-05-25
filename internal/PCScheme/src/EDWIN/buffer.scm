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

;;;; Buffer Abstraction

(define-named-structure "Buffer"
  name
  group
  point
  mark-ring
  modified?
  windows
  cursor-y
  pathname
  truename
  writeable?
  alist)

(define (make-buffer name)
  (let ((group (region-group (string->region ""))))
    (let ((buffer (%make-buffer)))
      (vector-set! buffer buffer-index:name name)
      (vector-set! buffer buffer-index:group group)
      (set-buffer-point! buffer (%group-start group))
      (vector-set! buffer buffer-index:mark-ring (make-ring 10))
      (ring-push! (buffer-mark-ring buffer) (%group-start group))
      (vector-set! buffer buffer-index:modified? #!FALSE)
      (vector-set! buffer buffer-index:windows '())
      (vector-set! buffer buffer-index:cursor-y #!FALSE)
      (vector-set! buffer buffer-index:pathname #!FALSE)
      (vector-set! buffer buffer-index:truename #!FALSE)
      (vector-set! buffer buffer-index:writeable? #!TRUE)
      (vector-set! buffer buffer-index:alist '())
      (let ((daemon (make-buffer-modification-daemon buffer)))
	(add-group-insert-daemon! group daemon)
	(add-group-delete-daemon! group daemon))
      buffer)))


(define (buffer-region buffer)
  (group-region (buffer-group buffer)))

(define (buffer-start buffer)
  (%group-start (buffer-group buffer)))

(define (buffer-end buffer)
  (%group-end (buffer-group buffer)))

(define (buffer-modeline-event! buffer type)
  (define (loop windows)
    (if (not (null? windows))
	(begin (window-modeline-event! (car windows) type)
	       (loop (cdr windows)))))
  (loop (buffer-windows buffer)))

(define (add-buffer-window! buffer window)
  (vector-set! buffer buffer-index:windows
	       (cons window (vector-ref buffer buffer-index:windows))))

(define (set-buffer-cursor-y! buffer cursor-y)
  (vector-set! buffer buffer-index:cursor-y cursor-y))

(define (set-buffer-name! buffer name)
  (vector-set! buffer buffer-index:name name)
  (buffer-modeline-event! buffer 'BUFFER-NAME))


(define (set-buffer-pathname! buffer pathname)
  (vector-set! buffer buffer-index:pathname pathname)
  (buffer-modeline-event! buffer 'BUFFER-PATHNAME))

(define (set-buffer-truename! buffer truename)
  (vector-set! buffer buffer-index:truename truename)
  (buffer-modeline-event! buffer 'BUFFER-TRUENAME))

(define (set-buffer-point! buffer mark)
  ;; Each window has its own point, so instead of signalling a point
  ;; change from here, the window's point is changed and it tells
  ;; the buffer about it.
  (vector-set! buffer buffer-index:point
                      (if (mark-left-inserting? mark)
                          mark
                          (%make-mark (mark-line mark) 
                                      (mark-position mark) #!true))))

(define ((make-buffer-modification-daemon buffer) . args)
  (buffer-modified! buffer)
  #!FALSE)

(define (buffer-not-modified! buffer)
  (set-buffer-modified! buffer #!FALSE))

(define (buffer-modified! buffer)
  (set-buffer-modified! buffer #!TRUE))

(define (set-buffer-modified! buffer sense)
  (vector-set! buffer buffer-index:modified? sense)
  (buffer-modeline-event! buffer 'BUFFER-MODIFIED))

(define (buffer-read-only? buffer)
  (group-read-only? (buffer-group buffer)))

(define (set-buffer-writeable! buffer)
  (set-group-writeable! (buffer-group buffer))
  (vector-set! buffer buffer-index:writeable? #!TRUE)
  (buffer-modeline-event! buffer 'BUFFER-MODIFIABLE))

(define (set-buffer-file-read-only! buffer)
  (set-group-writeable! (buffer-group buffer))
  (vector-set! buffer buffer-index:writeable? #!FALSE)
  (buffer-modeline-event! buffer 'BUFFER-MODIFIABLE))

(define (set-buffer-read-only! buffer)
  (set-group-read-only! (buffer-group buffer))
  (vector-set! buffer buffer-index:writeable? #!FALSE)
  (buffer-modeline-event! buffer 'BUFFER-MODIFIABLE))

;;; Not used currently so commented out

;;;(define (with-read-only-defeated mark thunk)
;;;  (let ((group (mark-group mark)))
;;;    (define read-only?)
;;;    (dynamic-wind (lambda ()
;;;		    (set! read-only? (group-read-only? group))
;;;		    (if read-only?
;;;			(set-group-writeable! group)))
;;;		  thunk
;;;		  (lambda ()
;;;		    (if read-only?
;;;			(set-group-read-only! group))))))
;;;
;;;



