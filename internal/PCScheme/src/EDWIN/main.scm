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

;;; miscellaneous routine for windows

(define save-console-contents
  (lambda ()
    (set! *pcs-contents* (%save-window 'console))))

(define restore-console-contents
  (lambda ()
    (let ((cursor-x (%reify-port 'console 1))
          (cursor-y (%reify-port 'console 0)))
      (%clear-window 'console)
      (%reify-port! 'console 1 cursor-x)
      (%reify-port! 'console 0 cursor-y)
      (%restore-window 'console *pcs-contents*))))

(define make-pcs-status-invisible
  (lambda ()
    (%reify-port! pcs-status-window 11
                  (%logand (%reify-port pcs-status-window 11) -9))))

(define make-pcs-status-visible
  (lambda ()
    (%reify-port! pcs-status-window 11
                  (%logior (%reify-port pcs-status-window 11) 8))))


(define move-editor-to-upper-half
  (lambda ()
    (%reify-port! buffer-screen 4 11)
    (%reify-port! modeline-screen 2 11)
    (%reify-port! typein-screen 2 12)))

(define move-editor-to-full
  (lambda ()
    (%reify-port! buffer-screen 4 23)
    (%reify-port! modeline-screen 2 23)
    (%reify-port! typein-screen 2 24)))

(define move-pcs-window-lower
  (lambda ()
    (%reify-port! 'console 2 13)
    (%reify-port! 'console 4 11)))

(define move-pcs-to-full
  (lambda ()
    (%reify-port! 'console 2 0)
    (%reify-port! 'console 4 24)))

(begin
(define-integrable editor:name 0)
(define-integrable editor:buffer-window 1)
(define-integrable editor:bufferset 2)
(define-integrable editor:kill-ring 3)
(define-integrable editor-bufferset
  (lambda (editor)
    (vector-ref editor editor:bufferset)))

(define-integrable editor-kill-ring
  (lambda (editor)
    (vector-ref editor editor:kill-ring)))

(define-integrable editor-buffer-window
  (lambda (editor)
    (vector-ref editor editor:buffer-window)))

(define-integrable editor-name
  (lambda (editor)
    (vector-ref editor editor:name)))

(define-integrable current-buffer-set
  (lambda()
    (editor-bufferset edwin-editor)))

(define-integrable current-kill-ring
  (lambda ()
    (editor-kill-ring edwin-editor)))

(define-integrable current-buffer-window
  (lambda ()
    (editor-buffer-window edwin-editor)))
)
;;; screens

(define make-screen
  (lambda (xl yl lin col)
    (let ((window (%make-window'())))
      (%reify-port! window 2 yl)
      (%reify-port! window 3 xl)
      (%reify-port! window 4 lin)
      (%reify-port! window 5 col)
      (%reify-port! window 8
                    (%logand -2 (%reify-port window 8)))
      window)))

(define buffer-screen (make-screen 0 0 23 80))
(define modeline-screen
  (let ((screen (make-screen 0 23 1 80)))
    (%reify-port! screen 7 (if (= pcs-machine-type 1) 28 120))))

(define typein-screen (make-screen 0 24 1 80))
(define blank-screen (make-screen 0 0 24 80))
;;; editor

(define initial-buffer-name "Main")

(define make-editor
  (lambda (name)
    (let ((vec (make-vector 4))
	  (init-buffer (make-buffer initial-buffer-name)))
      (let ((bufferset (make-bufferset init-buffer)))
        (vector-set! vec editor:name name)
        (vector-set! vec editor:buffer-window
                         (make-buffer-window buffer-screen init-buffer))
        (vector-set! vec editor:bufferset bufferset)
        (vector-set! vec editor:kill-ring (make-ring 10))
        vec))))