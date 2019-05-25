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


;;;; Current State

;;;; Windows
(begin

(define-integrable current-window
  (lambda ()
    (current-buffer-window)))


;;;; Buffers

(define-integrable buffer-list
  (lambda ()
    (bufferset-buffer-list (current-bufferset))))

(define-integrable buffer-names
  (lambda ()
    (bufferset-names (current-bufferset))))

(define-integrable current-buffer
  (lambda ()
    (window-buffer (current-window))))

(define-integrable find-buffer
  (lambda (name)
    (bufferset-find-buffer (current-bufferset) name)))

(define-integrable create-buffer
  (lambda (name)
    (bufferset-create-buffer (current-bufferset) name)))

(define-integrable find-or-create-buffer
  (lambda (name)
    (bufferset-find-or-create-buffer (current-bufferset) name)))

(define-integrable rename-buffer
  (lambda (buffer new-name)
    (bufferset-rename-buffer (current-bufferset) buffer new-name)))

(define-integrable current-point
  (lambda ()
    (window-point (current-window))))

(define-integrable set-current-point!
  (lambda (mark)
    (set-window-point! (current-window) mark)))

(define-integrable current-mark
  (lambda ()
    (buffer-mark (current-buffer))))

(define-integrable  set-current-mark!
  (lambda (mark)
    (set-buffer-mark! (current-buffer) mark)))

(define-integrable push-current-mark!
  (lambda (mark)
    (push-buffer-mark! (current-buffer) mark)))

(define-integrable pop-current-mark!
  (lambda ()
    (pop-buffer-mark! (current-buffer))))

(define-integrable current-region
  (lambda ()
    (make-region (current-point) (current-mark))))
)

;;; These have been commented out as are not currently used. However,
;;; these are useful routines and should not be deleted from this file.


;;;(define (kill-buffer buffer)
;;;  (let ((new-buffer (other-buffer buffer))
;;;	(current? (eq? buffer (current-buffer)))
;;;	(windows (buffer-windows buffer)))
;;;    (if (and (not new-buffer) (not (null? windows)))
;;;	(error "Buffer to be killed has no replacement" buffer))
;;;    (bufferset-kill-buffer! (current-bufferset) buffer)
;;;    (if current? (select-buffer new-buffer))
;;;    (mapc (lambda (window) (set-window-buffer! window new-buffer))
;;;	  windows)))

;;;(define (with-current-window new-window thunk)
;;;  (define old-window)
;;;  (dynamic-wind (lambda ()
;;;		  (set! old-window (current-window))
;;;		  (select-window (set! new-window)))
;;;		thunk
;;;		(lambda ()
;;;		  (set! new-window (current-window))
;;;		  (select-window (set! old-window)))))
;;;
;;;(define (with-selected-buffer buffer thunk)
;;;  (define old-buffer)
;;;  (dynamic-wind (lambda ()
;;;		  (set! old-buffer (current-buffer))
;;;		  (select-buffer-no-record buffer))
;;;		thunk
;;;		(lambda ()
;;;		  (set! buffer (current-buffer))
;;;		  (select-buffer-no-record old-buffer))))


;;;; Point and Mark

;;;(define (with-current-point new-point thunk)
;;;  (define old-point)
;;;  (dynamic-wind (lambda ()
;;;		  (set! old-point (current-point))
;;;		  (set-current-point! new-point))
;;;		thunk
;;;		(lambda ()
;;;		  (set! new-point (current-point))
;;;		  (set-current-point! old-point))))

(define (buffer-mark buffer)
  (let ((ring (buffer-mark-ring buffer)))
    (if (ring-empty? ring) (editor-error))
    (ring-ref ring 0)))

(define (set-buffer-mark! buffer mark)
  (ring-set! (buffer-mark-ring buffer)
	     0
	     (mark-right-inserting mark)))

(define (push-buffer-mark! buffer mark)
  (ring-push! (buffer-mark-ring buffer)
	      (mark-right-inserting mark)))

(define (pop-buffer-mark! buffer)
  (ring-pop! (buffer-mark-ring buffer)))

(define (set-current-region! region)
  (set-current-point! (region-start region))
  (push-current-mark! (region-end region)))

(define (set-current-region-reversed! region)
  (push-current-mark! (region-start region))
  (set-current-point! (region-end region)))
