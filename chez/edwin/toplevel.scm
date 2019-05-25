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

;;; toplevel

(define edwin-editor)
(define *pcs-contents* '())

(define edwin
  (letrec
   ((%edwin-reset
     (lambda ()
       (set! edwin-editor (make-editor "Edwin"))
       (reset-display)
       *the-non-printing-object*))

    (reset-display
     (lambda ()
       (reset-buffer-window (current-buffer-window))
       (reset-modeline-window)
       (reset-typein-window))))
   (lambda ()
     (call/cc
       (lambda (k)
	 (fluid-let ((editor-continuation k))
	   (save-console-contents)
	   (make-pcs-status-invisible)
	   (if (or (unassigned? edwin-editor)
		   (not edwin-editor))
	       (%edwin-reset)
	       (reset-display))
	   (top-level-command-reader)))))))

(define top-level-command-reader
  (lambda ()
    (letrec
      ((top-level-command-reader
	 (lambda ()
	   (catch
	    (lambda (k)
	      (fluid-let ((*error-continuation* k)
			  (*^G-continuation* k))
		(command-reader))))
	   (top-level-command-reader)))

       (command-reader
	 (lambda ()
	   (fluid-let ((*command-message* #!false))
	     (with-command-argument-reader
	      (lambda ()
		(command-reader-loop))))))

       (command-reader-loop
	 (lambda ()
	   (fluid-let ((*command-char* '())
		       (*command* '())
		       (*next-message* #!false))
	     (start-next-command)
	     (set-fluid! *command-message* (fluid *next-message*)))
	   (command-reader-loop )))

       (start-next-command
	 (lambda ()
	   (reset-command-argument-reader!)
	   (reset-command-prompt!)
	   (read-and-dispatch-on-char))))
      (top-level-command-reader))))

(define (throw continuation value)
  (continuation value))

(define (abort-current-command)
  (throw (error-continuation) 'abort))

(define (error-continuation)
  (fluid *error-continuation*))

(define (editor-error . msg)
  (beep)
  (if msg (temporary-message (car msg)))
  (abort-current-command))

(define (read-and-dispatch-on-char)
  (dispatch-on-char (editor-read-char (window-screen (current-window)))))

(define ^G-char (integer->char 7))

(define editor-read-char
  (lambda (screen)
    (if (not (char-ready? screen))
	(begin
	  (update-display! (current-window))
	  (update-modeline!)))
    (if (not (eq? screen typein-screen))
	(if (or (not (char-ready?))
		(delay-input 50 screen))
	    (update-typein-window!)))
    (let ((char (read-char screen)))
	 (cond ((eq? char ^G-char) (editor-error "Abort"))
	       ((eof-object? char) ^Z-char)
	       (else char)))))

(define (dispatch-on-char char)
   (set-fluid! *command-char* char)
   (set-command-prompt!
    (string-append-separated (command-argument-prompt)
			     (obj->string char)))
  (dispatch-on-command (comtab-entry char) char))

(define (dispatch-on-command command char)
  (set-fluid! *command* command)
  (let ((procedure command)
	(argument
	 (or (command-argument-value)
	     (and (command-argument-negative?) -1))))
    (if (or argument)
        ;; The C-U for numeric arguments has already reset the paren cache,
        ;; so no need to do anything further about it here.
        (procedure argument)
        ;; Reset the paren-cache on any non-insert or left-paren command.
        ;; Be careful we *don't* reset it on right-paren.
	(cond ((eq? procedure ^r-insert-self-command)
               (and (char=? #\( char) (cache-paren-mark '()))   ;;;;;)  3.02
	       (let ((window (current-window))
		     (point (current-point)))
		 (if (and (buffer-modified? (window-buffer window))
			  (line-end? point)
			  (char-graphic? char)
			  (< (window-point-x window)
			     (-1+ (window-x-size window))))
		     (begin (%region-insert-char! (mark-line point)
						  (mark-position point)
						  char)
			    (direct-output-for-insert! window
						       char))
		     (region-insert-char! point char))))
	      ((eq? procedure ^r-forward-character-command)
               (cache-paren-mark '())				;3.02
	       (let ((window (current-window))
		     (point (current-point)))
		 (if (and (not (group-end? point))
			  (char-graphic? (mark-right-char point))
			  (< (window-point-x window)
			     (- 2 (window-x-size window))))
			     ;;; to take care of continuation lines
		     (direct-output-forward-character! window)
		     (procedure argument))))
	      ((eq? procedure ^r-backward-character-command)
               (cache-paren-mark '())				;3.02
	       (let ((window (current-window))
		     (point (current-point)))
		 (if (and (not (group-start? point))
			  (char-graphic? (mark-left-char point))
			  ;; Use 1 instead of 0 so we don't have
			  ;; to worry about continuation lines.
			  (> (window-point-x window) 1))
		     (direct-output-backward-character! window)
		     (procedure argument))))
              ((eq? procedure ^r-lisp-insert-paren-command)  	;3.02
               (procedure argument))				;3.02
	      (else
               (cache-paren-mark '())				;3.02
	       (procedure argument))))))

(define (current-command-char)
  (fluid *command-char*))

(define (current-command)
  (fluid *command*))

(define (set-command-message! tag . arguments)
  (set-fluid! *next-message* (cons tag arguments)))

(define (command-message-receive tag if-received if-not-received)
  (if (and (fluid *command-message*)
	   (eq? (car (fluid *command-message*)) tag))
      (apply if-received (cdr (fluid *command-message*)))
      (if-not-received)))

(define (beep)
  (princ ^G-char typein-screen))







