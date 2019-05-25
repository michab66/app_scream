;      -*- Mode: Lisp -*-			      Filename:  pstl.s

;--------------------------------------------------------------------------;
;									   ;
;			  TI SCHEME -- PCS Compiler			   ;
;		   Copyright 1985 (c) Texas Instruments 		   ;
;									   ;
;			       David Bartley				   ;
;									   ;
;		   Standard  SCHEME-Top-Level  Routines 		   ;
;									   ;
;--------------------------------------------------------------------------;

; Revision history:
;  6/01/87 tc - Modified original PSTL.S so that only top level functions
;		are now in this file.
;  6/01/87 rb - modified runtime-system toplevel handling so it works
;		identically to the compiler version; this gets rid of
;		APPLICATION-TOP-LEVEL, and PATCH.PCS and .INI handling
;		will get executed in the runtime system

;define standard toplevel loop and support functions

(begin
  (define reset-scheme-top-level				; SCHEME-TOP-LEVEL
    (let ((saved-genv user-initial-environment))
      (lambda ()
	(letrec
	 ((==reset== '())
	  (==scheme-reset==			; here for SCHEME-RESET
	   (lambda ()
	     (%set-global-environment saved-genv)
	     (set! (fluid input-port) standard-input)
	     (set! (fluid output-port) standard-output)
	     (putprop '%PCS-STL-HISTORY (list '()) %pcs-stl-history)
	     (newline)
	     (display "[PCS-DEBUG-MODE is ")
	     (display (if pcs-debug-mode "ON]" "OFF]"))
	     (newline)
	     (call/cc (lambda (k)
			(set! ==reset== (lambda ()(k '())))
			(set! (fluid scheme-top-level)
			      ==reset==)))
						; here for RESET (if fluid
						; SCHEME-TOP-LEVEL hasn't been redefined;
						; if it has, restart that function)
	     (pcs-kill-engine)
	     (gc)			; restore WHO line  (temporary)
	     (more)))
	  (more
	   (lambda ()
	     (pcs-clear-registers)
	     (fresh-line)
	     (display "[")
	     (display (length (getprop '%PCS-STL-HISTORY %pcs-stl-history)))
	     (display "] ")
	     (let ((problem (read)))
	       (flush-input)
	       (if (eof-object? problem)
		   (display "[End of file read by SCHEME-TOP-LEVEL]")
		   (begin
		     (putprop '%PCS-STL-HISTORY
			      (cons (list problem)
				    (getprop '%PCS-STL-HISTORY
					     %pcs-stl-history))
			      %pcs-stl-history)
		     (let* ((answer (eval (if %pcs-stl-debug-flag
					      (compile (list 'BEGIN
							 '(%BEGIN-DEBUG)
							 problem))
					      problem)))
			    (next (fluid scheme-top-level)))
		       (when (not (eq? answer *the-non-printing-object*))
			     (write answer))
		       (putprop '%PCS-STL-HISTORY
				(cons (cons problem answer)
				      (cdr (getprop '%PCS-STL-HISTORY
						    %pcs-stl-history)))
				%pcs-stl-history)
		       (if (eq? next ==reset==)
			   (more)
			   (next)))))))))
	 (set! (fluid scheme-top-level) ==scheme-reset==)
	 *the-non-printing-object*))))

  ;;; %C accesses the nth user command
  ;;; %D accesses the result of the nth user command

  (define %c						; %C
    (lambda (n)
      (let ((history (getprop '%PCS-STL-HISTORY %pcs-stl-history)))
	(and (positive? n)
	     (< n (length history))
	     (car (list-ref (reverse history) n))))))

  (define %d						; %D
    (lambda (n)
      (let ((history (getprop '%PCS-STL-HISTORY %pcs-stl-history)))
	(and (positive? n)
	     (< n (length history))
	     (cdr (list-ref (reverse history) n))))))
) ;begin

(reset-scheme-top-level)

(let ((file (%system-file-name "PATCH.PCS")))
  (when (file-exists? file)			 ; system patches
	(load file)))


;; Pathnames read as text from a file will have single backslashes.
;; This doubles them so a read-from-string type operation will work on them.
;; It's used for the .INI processing following.
(define (double-slashify string)
  (let loop ((m 0)
	     (n 0)
	     (new (make-string (string-length string) nil)))
    (if (= m (string-length string))
	new
	(begin
	  (string-set! new n (string-ref string m))
	  (if (char=? (string-ref string m) #\\)
	      (let ((newer (make-string (add1 (string-length new)) nil)))
		(substring-move-left! new 0 (+ n 1) newer 0)
		(string-set! newer (+ n 1) #\\)
		(loop (+ m 1) (+ n 2) newer))
	      (loop (+ m 1) (+ n 1) new))))))


(%set-global-environment user-initial-environment)


;; Note:  You can make your own toplevel function the system's toplevel by
;; assigning it to the fluid variable SCHEME-TOP-LEVEL from the .INI file.
;; Don't invoke it yourself.  After loading the .INI file, this file's
;; final SCHEME-RESET initializes the VM for toplevel recovery
;; (in case of errors) and invokes the toplevel function automatically.


(cond ((null? pcs-initial-arguments)	      ;no args at all, use scheme.ini
       (when (file-exists? "scheme.ini")
	     (load "scheme.ini")))
      (else
	(let ((pia-files
		(map symbol->string
		     (let ((x (read (open-input-string
				      (double-slashify (car pcs-initial-arguments))))))
		       (if (pair? x) x (list x))))))	;handle nonlist file
	  (let loop ((rest pia-files) (ini-files '()))  ;handle list files
	    (let ((f (car rest)))
	      (cond ((null? rest)
		     (when (null? ini-files)	    ;no ini's given, use scheme.ini
			   (set! ini-files '("scheme.ini")))
		     (for-each		    ;load several ini's
		       (lambda (f)
			 (when (file-exists? f) (load f)))
		       ini-files))
		    ((< (string-length f) 4)	    ;file sans extension--assumed ini
		     (loop (cdr rest) (cons f ini-files)))
		    ((substring-ci=? f (- (string-length f) 4) (string-length f)
				     ".app" 0 4)
		     (loop (cdr rest) ini-files))  ;don't reload compiler
		    ((substring-ci=? f (- (string-length f) 4) (string-length f)
				     ".xli" 0 4)
		     (loop (cdr rest) ini-files))  ;ignore XLI files
		    (else
		      (loop (cdr rest) (cons f ini-files))) ;assume fasl file
		    ))))))


(scheme-reset)		; must be last operation!
