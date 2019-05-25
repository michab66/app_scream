;      -*- Mode: Lisp -*-			      Filename:  pstd2.s

;--------------------------------------------------------------------------;
;									   ;
;			  TI SCHEME -- PCS Compiler			   ;
;		   Copyright 1985 (c) Texas Instruments 		   ;
;									   ;
;			       Terry Caudill				   ;
;									   ;
;		   Scheme Standard Functions and Definitions     	   ;
;									   ;
;--------------------------------------------------------------------------;

; Revision history:
;  6/01/85 87 - Modified from former PSTL file
;
;  6/01/87 rb - added %XLI-DEBUG

;;;			     Scheme 84 ENGINES

(define pcs-null-k
  (lambda (ticks eng)
    (error "Null continuation invoked")))

(define pcs-success-k pcs-null-k)

(define pcs-fail-k '())

(define pcs-engine-timeout
  (lambda ()
    (call/cc (lambda (k)
	       (let ((fail pcs-fail-k))
		 (set! pcs-success-k pcs-null-k)
		 (set! pcs-fail-k '())          ; help GC
		 (fail (make-engine (lambda () (k '())))))))))

(define pcs-kill-engine
  (lambda ()
    (when (not (eq? pcs-success-k pcs-null-k))
	  (%stop-timer)
	  (set! pcs-success-k pcs-null-k)
	  (set! pcs-fail-k '())                 ; help GC
	  (display "[Current engine has been killed]")
	  (newline))))

;;; ``The solution to the engine tail recursion problem is to wrap the
;;; CALL/CC application in MAKE-ENGINE in an application and pass thunks to
;;; ENGINE-K.  This is a very important trick to learn about CALL/CC.
;;; Serious CALL/CC hackers should study it carefully.''
;;;
;;;						 -- Chris Haynes, 10/2/85

(define make-engine
  (lambda (thunk)
    (if (proc? thunk)
	(lambda (ticks sk fk)
	  ((call/cc
	      (lambda (engine-k)
		(when (not (eq? pcs-success-k pcs-null-k))
		      (error "Engine already running"))
		(when (or (not (integer? ticks))
			  (not (proc? sk))
			  (not (proc? fk)))
		      (error "Invalid argument to <engine>" ticks sk fk))
		(set! pcs-success-k
		      (lambda (v ticks) (engine-k (lambda () (sk v ticks)))))
		(set! pcs-fail-k
		      (lambda (v) (engine-k (lambda () (fk v)))))
		(%start-timer ticks)
		(let* ((result (thunk))
		       (ticks (%stop-timer)))
		  (%stop-timer)
		  (set! pcs-success-k pcs-null-k)
		  (set! pcs-fail-k '())                 ; help gc
		  (error "ENGINE-RETURN not invoked"))))))
	(%error-invalid-operand 'MAKE-ENGINE thunk))))

(define engine-return
  (lambda (value)
    (let* ((ticks (%stop-timer))
	   (sk pcs-success-k))
      (%stop-timer)
      (set! pcs-success-k pcs-null-k)
      (set! pcs-fail-k '())                             ; help gc
      (sk value ticks))))

;;;
;;;	Miscellaneous Functions
;;;

(define freesp						; FREESP
  (lambda ()
    (%esc1 3)))

(define %hash						; %HASH
  (lambda (symbol)
    (%esc2 9 (symbol->string symbol))))

(define get-gc-compact-count				; GET-GC-COMPACT-COUNT
  (lambda ()
    (%esc1 21)))

(define set-gc-compact-count!				; SET-GC-COMPACT-COUNT!
  (lambda (value)
    (if (not (integer? value))
      (%error-invalid-operand 'set-gc-compact-count! value)	
      (%esc2 22 value))))

; 0 = off; 1 = on
(define %xli-debug					; %XLI-DEBUG
  (lambda (x)
    (%esc2 18 x)))

(define %system-file-name				; %SYSTEM-FILE-NAME
  (lambda (name)
    (let* ((dir pcs-sysdir)
	   (len (string-length dir)))
      (if (zero? len)
	  name
	  (string-append
	      (if (char=? (string-ref dir (- len 1)) #\\)
		  dir
		  (string-append dir "\\"))
	      name)))))

;;;
;;;	Miscellaneous Error type Functions
;;;

(define %error-invalid-operand			; %ERROR-INVALID-OPERAND
  (lambda (name opd)
    (error (string-append "Invalid argument to "
			  (symbol->string name))
	   opd)))


(define %error-invalid-operand-list		; %ERROR-INVALID-OPERAND-LIST
  (lambda (name . opds)
    (error (string-append "Invalid argument list for "
			  (symbol->string name))
	   (cons name opds))))


(define syntax-error					; SYNTAX-ERROR
  (letrec ((prin (lambda (x)
		   (newline)(write x))))
    (lambda args
      (newline)
      (display "[Syntax Error] ")
      (display (car args))
      (mapc prin (cdr args))
      (newline)
      (display "[Returning to top level]")
      (newline)
      (reset))))


(define pcs-clear-registers				; PCS-CLEAR-REGISTERS
  (lambda ()
    ;; do NOT define with DEFINE-INTEGRABLE !!
    (%clear-registers)	; calling this routine saves
    '()))               ; needed registers first


(define pcs-make-label					; PCS-MAKE-LABEL
  (lambda (name)
    (set! pcs-local-var-count (+ pcs-local-var-count 1))
    (cons pcs-local-var-count name)))


;;;
;;;	Miscellaneous System Definitions
;;;

(begin
  (define pcs-gc-message nil)   ;nil says use system defaults
  (define pcs-gc-reset nil)

  (define standard-input      'CONSOLE)
  (define standard-output     'CONSOLE)
  (define false 	      #!false)
  (define true		      #!true)
  (define the-empty-stream    (vector 'THE-EMPTY-STREAM))

  (define pcs-error-flag	     #!false)
  (define pcs-binary-output	     #!true)


  (define *error-code*	    0)		; force these to be allocated
  (define *error-message*   '())        ; in USER-GLOBAL-ENVIRONMENT
  (define *irritant*	    0)
  (define *user-error-handler* '())
) ;begin
