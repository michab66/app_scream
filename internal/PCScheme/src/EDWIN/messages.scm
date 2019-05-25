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

(define reset-typein-window
  (lambda ()
    (%clear-window typein-screen)))

 ;;; command-prompts

(define *command-prompt-string* #!false)

(define *command-prompt-displayed?* #!false)

(define *temporary-message-displayed?* #!false)
 
(define *prompt-should-be-erased?* #!false)

(define *t-msg* "")

(define reset-command-prompt!
  (lambda ()
    (set! *command-prompt-string* #!false)
    (set! *command-prompt-displayed?* #!false)))

(define set-command-prompt!
  (lambda (prompt)
    (set! *command-prompt-string* prompt)))

(define set-echo-prompt!
  (lambda (string)
    (set! *command-prompt-string* #!false)
    (set! *command-prompt-displayed?* #!false)
    (set! *temporary-message-displayed?* #!false)
    (set! *prompt-should-be-erased?* #!false)
    (write-prompt! string)))

(define erase-echo-prompt!
  (lambda ()
    (set! *command-prompt-string* #!false)
    (set! *command-prompt-displayed?* #!false)
    (set! *temporary-message-displayed?* #!false)
    (set! *prompt-should-be-erased?* #!false)
    (clear-prompt!)))

(define update-typein-window!
  (lambda ()
    (cond (*command-prompt-string* 
             (write-prompt! *command-prompt-string*)
             (set! *command-prompt-string* #!false)
             (set! *command-prompt-displayed?* #!true)
             (set! *temporary-message-displayed?* #!false)
             (set! *prompt-should-be-erased?* #!true))

          (*prompt-should-be-erased?*  
             (set! *command-prompt-displayed?* #!false)
             (set! *temporary-message-displayed?* #!false)
             (set! *prompt-should-be-erased?* #!false)
             (clear-prompt!))
   
          (*temporary-message-displayed?* 
             (set! *prompt-should-be-erased?* #!true)
             (set! *command-prompt-displayed?* #!false)
             (set! *temporary-message-displayed?* #!false)))))

(define write-prompt!
  (lambda (string)
    (%clear-window typein-screen)
    (write-string! typein-screen string 0 0)))

(define clear-prompt!
  (lambda ()
    (%clear-window typein-screen)))

(define temporary-message
  (lambda (string)
    (set! *t-msg* string)
    (set-temp-message-status)
    (write-prompt! string)))

(define set-temp-message-status
  (lambda ()
    (set! *command-prompt-string* #!false)
    (set! *command-prompt-displayed?* #!false)
    (set! *prompt-should-be-erased?* #!false)
    (set! *temporary-message-displayed?* #!true)))

(define append-message
  (lambda (string)
    (set! *t-msg* (string-append *t-msg* string))
    (temporary-message *t-msg*)))

 ;;; prompting

(define prompt-for-pathname
  (lambda (prompt)
    (temporary-message prompt)
    (read-pathname-from-screen typein-screen)))

(define prompt-for-confirmation?
  (lambda (prompt)
    (define (loop)
      (let ((char (char-upcase (editor-read-char typein-screen))))
        (if (or (char=? #\Y char) (char=? #\N char))
            (char=? #\Y char)
            (loop))))
    (temporary-message prompt)
    (loop)))


(define read-pathname-from-screen
  (let ((input-buffer (make-string 80 #\space)))
    (lambda (screen)
      (define erase-move-back
	(lambda (screen)
	  (let ((cursor-x (%reify-port screen screen:cursor-x))
		(cursor-y (%reify-port screen screen:cursor-y))
		(set-cursor-pos
		 (lambda (x y)
		   (%reify-port! screen screen:cursor-x x)
		   (%reify-port! screen screen:cursor-y y))))
	    (set-cursor-pos (-1+ cursor-x) cursor-y)
	    (princ #\space screen)
	    (set-cursor-pos (-1+ cursor-x) cursor-y))))

      (define (loop char ptr)
	(cond ((char=? char #\return) (substring input-buffer 0 ptr))
	      ((char=? char #\Backspace)
               (if (not (= ptr 0))
                   (begin
                     (erase-move-back screen)
		     (loop (editor-read-char screen) (-1+ ptr)))
		   (loop (editor-read-char screen) ptr)))
              ((char-graphic? char)
	       (princ char screen)
	       (string-set! input-buffer ptr char)
	       (loop (editor-read-char screen) (1+ ptr)))
              (else (loop (editor-read-char screen) ptr))))
	(loop (editor-read-char screen) 0))))

