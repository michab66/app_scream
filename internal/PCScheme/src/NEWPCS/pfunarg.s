
;      -*- Mode: Lisp -*-			     Filename:	pfunarg.s

;		      Last Revision:  12-Nov-85 1100ct

;--------------------------------------------------------------------------;
;									   ;
;			  TI SCHEME -- PCS Compiler			   ;
;		   Copyright 1985 (c) Texas Instruments 		   ;
;									   ;
;			       David Bartley				   ;
;									   ;
;		    "Funarg" Backups for PCS Primitives                    ;
;									   ;
;				   NOTE:				   ;
;									   ;
;	Most of these routines are defined in terms of primitive	   ;
;	operations with the same name.	Thus, they must be compiled	   ;
;	with PCS-INTEGRATE-PRIMITIVES set true.  Also, be sure not to	   ;
;	use DEFREC!, LETREC, REC, etc., incorrectly.			   ;
;									   ;
;	LAST UPDATE:							   ;
;	  4/13/87 TC - Funarg handler for make-string			   ;
;--------------------------------------------------------------------------;


(define *						; *
  (lambda args	; for funarg use, don't use DEFREC!
    (cond ((null? args)
	   1)
	  (t (do ((a (car args) (* a (car x)))
		  (x (cdr args) (cdr x)))
		 ((null? x) a))))))


(define +						; +
  (lambda args	; for funarg use, don't use DEFREC!
    (cond ((null? args)
	   0)
	  (t (do ((a (car args) (+ a (car x)))
		  (x (cdr args) (cdr x)))
		 ((null? x) a))))))


(define -						; -
  (lambda args	; for funarg use, don't use DEFREC!
    (cond ((null? args)
	   0)
	  ((null? (cdr args))
	   (- (car args)))
	  (t (do ((a (car args) (- a (car x)))
		  (x (cdr args) (cdr x)))
		 ((null? x) a))))))


(define /						; /
  (lambda args	; for funarg use, don't use DEFREC!
    (cond ((null? args)
	   1)
	  ((null? (cdr args))
	   (/ 1 (car args)))
	  (t (do ((a (car args) (/ a (car x)))
		  (x (cdr args) (cdr x)))
		 ((null? x) a))))))


(define append						; APPEND
  (letrec		; for funarg use
    ((append*
      (lambda (args)
	(cond ((null? args)
	       '())
	      ((null? (cdr args))
	       (car args))
	      ((null? (cddr args))
	       (%append (car args)(cadr args)))
	      (else
	       (%append (car args) (append* (cdr args))))))))
    (lambda args
      (append* args))))


(define append! 					; APPEND!
  (letrec		; for funarg use
    ((append!*		; don't use DEFREC!
      (lambda (args)
	(cond ((null? args)
	       '())
	      ((null? (cdr args))
	       (car args))
	      ((null? (cddr args))
	       (append! (car args) (cadr args)))
	      (else
	       (append! (car args) (append!* (cdr args))))))))
    (lambda args
       (append!* args))))

(define char-ready?					; CHAR-READY?
  (lambda args			; for funarg uses
    (char-ready? (car args))))	; don't define with defrec!


(define display 					; DISPLAY
  (lambda (exp . rest)		; for funarg uses
    (display exp		; don't define with defrec!
	     (car rest))))


(define list						; LIST
  (lambda x x))   ; (for funarg use)


(define list*						; LIST*
  (lambda x	  ; (for funarg use)
    (let loop ((x x))
	 (cond ((atom? x)	x)
	       ((atom? (cdr x)) (car x))
	       (else (cons (car x) (loop (cdr x))))))))


(define make-vector					; MAKE-VECTOR
  (lambda (size . rest)  ; for funarg use, don't use DEFREC!
    (let ((v (make-vector size)))
      (when rest
	    (vector-fill! v (car rest)))
      v)))

(define make-string					; MAKE-STRING
  (lambda (size . rest)  ; for funarg use, don't use DEFREC!
    (make-string size		; don't define with defrec!
		 (car rest))))


(define max						; MAX
  (lambda args	; for funarg use, don't use DEFREC!
    (if (null? args)
	0
	(do ((a (car args) (max a (car x)))
	     (x (cdr args) (cdr x)))
	    ((null? x) a)))))


(define min						; MIN
  (lambda args	; for funarg use, don't use DEFREC!
    (if (null? args)
	0
	(do ((a (car args) (min a (car x)))
	     (x (cdr args) (cdr x)))
	    ((null? x) a)))))


(define newline 					; NEWLINE
  (lambda args			; for funarg uses
    (newline (car args))))	; don't define with defrec!


(define prin1						; PRIN1
  (lambda (exp . rest)		; for funarg uses
    (prin1 exp (car rest))))	; don't define with defrec!


(define princ						; PRINC
  (lambda (exp . rest)		; for funarg uses
    (princ exp (car rest))))	; don't define with defrec!


(define print						; PRINT
  (lambda (exp . rest)		; for funarg uses
    (print exp (car rest))))	; don't define with defrec!


(define read-line					; READ-LINE
  (lambda args			; for funarg uses
    (read-line (car args))))	; don't define with defrec!


(define read-atom					; READ-ATOM
  (lambda args			; for funarg uses
    (read-atom (car args))))	; don't define with defrec!


(define read-char					; READ-CHAR
  (lambda args			; for funarg uses
    (read-char (car args))))	; don't define with defrec!

							; STRING-APPEND
;; STRING-APPEND should be moved here from PCHREQ.S
;; (for funarg definition) for consistency

(define vector						; VECTOR
  (lambda L
    (list->vector L)))


(define write						; WRITE
  (lambda (exp . rest)		; for funarg uses
    (write exp (car rest))))	; don't define with defrec!

(define write-char					; WRITE-CHAR
  (lambda (exp . rest)		    ; for funarg uses
    (write-char exp (car rest))))   ; don't define with defrec

(define %xesc						; %XESC (XLI)
  (lambda (length name . rest)
    (%execute (compile `(%xesc ,length ,name ,@rest)))))
