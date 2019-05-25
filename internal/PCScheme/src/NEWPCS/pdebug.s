;      -*- Mode: Lisp -*-			    Filename:  pdebug.s

;--------------------------------------------------------------------------;
;									   ;
;			  TI SCHEME -- PCS Compiler			   ;
;		   Copyright 1985 (c) Texas Instruments 		   ;
;									   ;
;			       David Bartley				   ;
;									   ;
;		    System Debugger and Error Handlers			   ;
;									   ;
;--------------------------------------------------------------------------;

; Revision history:
; db 10/18/85 - ??
; tc 03/13/87 - Extended errors for DOS I/O errors 
		  

; The following definitions are used only at compile time for readability 
; and understanding. They will not be written out to the .so file.
; See pboot.s and compile.all.

(compile-time-alias IO-ERRORS-START 21)
(compile-time-alias IO-ERRORS-END   108)
(compile-time-alias DOS-IO-ERROR    21)
(compile-time-alias FILE-NOT-FOUND  22)
(compile-time-alias PATH-NOT-FOUND  23)
(compile-time-alias TOO-MANY-FILES  24)

(define assert-procedure)
(define breakpoint-procedure)
(define error-procedure)
(define *error-handler*)

(letrec
 ((uv-msg
     '(1 2 3 4))
  (msg-codes
     '((0 . "Unspecified VM error")
       (1 . "Variable not defined in current environment")
       (2 . "SET! of an unbound variable")
       (3 . "Variable not defined in lexical environment")
       (4 . "SET! of an unbound lexical variable")
       (5 . "Variable not defined in fluid environment")
       (6 . "SET-FLUID! of an unbound fluid variable")
       (7 . "Vector index out of range")
       (8 . "String index out of range")
       (9 . "Invalid substring range")                  ; not generated
       (10 . "Invalid operand to VM instruction")
       (11 . "User keyboard interrupt")
       (12 . "Attempt to call a non-procedural object")
   ;;  (13 . "Engine Timer Interrupt")
       (14 . "I/O attempted to a de-exposed window")
       ;; 14 is a trap for a window handler, not a real error
       (15 . "FLONUM overflow or underflow")
       (16 . "Divide by zero")
       (17 . "Non-numeric operand to arithmetic operation")
       (18 . "Register overflow--Too many arguments to closure")
       (19 . "MAKE-VECTOR size limit exceeded")
       (20 . "MAKE-STRING size limit exceeded")
       (21 . "DOS I/O error number ")
       (22 . "DOS I/O error - File not found")
       (23 . "DOS I/O error - Path not found")		
       (24 . "DOS I/O error - Too many open files")
       (25 . "DOS I/O error - Access denied")
       (32 . "DOS I/O error - Invalid access")
       (36 . "DOS I/O error - Invalid disk drive")
       (39 . "DOS I/O error - Disk write protected")
       (41 . "DOS I/O error - Drive not ready")
       (48 . "DOS I/O error - Printer out of paper")
       (200 . "DOS I/O error - Disk Full")
	))
  (oops
   (lambda (msg irritant env stk-index kind error-code)
     (fluid-let ((input-port standard-input)
		 (output-port standard-output))
       (let* ((si (if (negative? stk-index)
		      (%reify-stack (+ (%reify-stack -1) 6))
		      stk-index))
	      (env (if (null? env)
		       (%reify-stack (+ si 9))
		       env)))
	 (newline)
	 (display kind)
	 (when msg (display msg))
	 (newline)
	 (write irritant)
	 (newline)
	 (pcs-kill-engine)

	 (if (unbound? compile)
	   ;; see if compiler auto-loadable
	   (when (not (pcs-autoload-binding 'compile))
	     ;; Cant find compiler, punt
	     (display (integer->char 7))	  ;beep
	     (display "Press a key to return to toplevel, escape to exit to DOS")
	     (let ((ch (read-char)))
	       (if (char=? ch #\escape)
		 (exit)
	         (scheme-reset))))
	 ;else
	   (if (null? (%env-lu '%inspector user-initial-environment))
	     ;; check to see if we can load the inspector
	     (when (or (eqv? *error-message* TOO-MANY-FILES)
		       (null? (pcs-autoload-binding '%inspector)))
	       (display "Unable to autoload the inspector - file PINSPECT.FSL")
	       (reset))))	

         (%inspector msg kind irritant env si error-code)

       ))))
  (envoke-handler
   (lambda (number msg irritant stk-index err-code)
     (let ((handler (lambda ()
		       (oops msg 
			     irritant 
			     '() 
			     stk-index 
		             "[VM ERROR encountered!] " 
			     err-code))))
       (if (closure? *user-error-handler*)
	  (*user-error-handler* number
				msg
				irritant
				handler)
	  (handler)))))
  (decipher-error
   (lambda (stk-index)
     (let  ((err-code *error-code*)
	    (irritant *irritant*)
	    (err-num (and (number? *error-message*) *error-message*))
	    (msg     (apply-if (assv *error-message* msg-codes)
				      cdr
				      *error-message*)))
       (cond ((eqv? err-num 11)         		; Shift Break
	      (set! err-num 100))
	     ((and err-num  		  		; I/O Errors
	           (>= err-num IO-ERRORS-START)
	           (<= err-num IO-ERRORS-END))
	      (if (and (or (=? err-num FILE-NOT-FOUND)
	                   (=? err-num PATH-NOT-FOUND))
	               (fluid-bound? *file-exists-open*))
	        ((fluid *file-exists-open*) #!false))   ; error continuation

	      (set! err-num (- err-num (-1+ DOS-IO-ERROR)))
	      (if (number? msg)
		(set! msg (string-append (cdr (assv DOS-IO-ERROR msg-codes))
					 (integer->string err-num 10))))))
       (envoke-handler err-num msg irritant stk-index err-code))))
  ) ; letrec vars 

 (begin
   (set! assert-procedure				; ASSERT-PROCEDURE
     (lambda (msgs env)
       (oops '() (cons 'ASSERT (cons '() msgs)) env -1 "[ASSERT failure!] " 0)))

   (set! breakpoint-procedure				; BREAKPOINT-PROCEDURE
     (lambda (msg irritant env . rest)
       (let* ((stk-index (if (or (null? rest)
				 (not (integer? (car rest))))
			    -1
			    (car rest))))
         (oops msg irritant env stk-index "[BKPT encountered!] " 0))))

   (set! error-procedure				; ERROR-PROCEDURE
     (lambda (msg irritant env)
       (let ((system-error-handler
	       (lambda ()	
                 (oops msg irritant env -1 "[ERROR encountered!] " 0))))
         (if (closure? *user-error-handler*)
	   (begin
	     (*user-error-handler* '() msg irritant system-error-handler))
	 ;else
	   (system-error-handler)))))	

   (set! *error-handler*				; *ERROR-HANDLER*
     (lambda ()
       (cond ((and (zero? *error-code*) 		        ; resumable
		   (memv *error-message* uv-msg))	        ; unbound symbol
	      (if (pcs-autoload-binding *irritant*)
	        '()                                        ; autoload worked!
	      ;else
	        (let ((info (getprop *irritant* 'PCS*PRIMOP-HANDLER))
		      (compiler-present (or (not (unbound? compile))
					    (pcs-autoload-binding 'compile))))
	          (cond ((and compiler-present 
			      (integer? info)
			      (getprop *irritant* 'PCS*OPCODE))
		         (let* ((vars '(J I H G F E D C B A))
			        (bvl  (list-tail vars (- (length vars) info)))
			        (form `(define ,*irritant*
				          (lambda ,bvl
					     (,*irritant* . ,bvl))))
			        (dw pcs-display-warnings)
			        (ip pcs-integrate-primitives))
			   (set! pcs-display-warnings #!false)
			   (set! pcs-integrate-primitives #!true)
			   (eval form user-global-environment)
			   (set! pcs-display-warnings dw)
			   (set! pcs-integrate-primitives ip)
			   '()))
		        ((and compiler-present
			      (pair? info)
			      (eq? (car info) 'DEFINE-INTEGRABLE))
		         (let ((form `(define ,*irritant* ,(cdr info)))
			       (dw pcs-display-warnings)
			       (ip pcs-integrate-primitives))
			   (set! pcs-display-warnings #!false)
			   (set! pcs-integrate-primitives #!true)
			   (eval form user-initial-environment)
			   (set! pcs-display-warnings dw)
			   (set! pcs-integrate-primitives ip)
			   '()))
		        (else
		         (set! *error-message* 
			       (cdr (assv *error-message* msg-codes)))
		         (*error-handler*))))))
	     ((eqv? *error-message* 13)
	      (pcs-engine-timeout))				; Engine Timeout
	     (else
	      (decipher-error (%reify-stack
			         (+ (%reify-stack 
				       (+ (%reify-stack -1) 6)) 6)))))
     ) ;lambda
   ) ;set!
 ) ;begin
) ;letrec

(define autoload-from-file				; AUTOLOAD-FROM-FILE
  (lambda (file names . rest)
    (let ((env (if rest (car rest) user-initial-environment)))
      (putprop 'PCS-AUTOLOAD-INFO
	       (cons (list file names env)
		     (getprop 'PCS-AUTOLOAD-INFO
			      'PCS-AUTOLOAD-INFO))
	       'PCS-AUTOLOAD-INFO)
      '())))



(define pcs-autoload-binding '())			; PCS-AUTOLOAD-BINDING
(define remove-autoload-info '())			; REMOVE-AUTOLOAD-INFO

(letrec
  ((find-entry
      (lambda (name info)
	(and info
	     (or (symbol? name) (string? name))
	     (find-item name (caar info)(cadar info) info))))
   (find-item
      (lambda (name file symbols info)
	(cond ((string? name)
	       (if (string-ci=? name file)
	         (car info)
		 (find-entry name (cdr info))))
	      ((null? symbols)
	       (find-entry name (cdr info)))
	      ((eq? name (car symbols))
	       (car info))
	      (else
	       (find-item name file (cdr symbols) info))))))
    (set! pcs-autoload-binding
      (lambda (name)
        (let* ((info (getprop 'PCS-AUTOLOAD-INFO 'PCS-AUTOLOAD-INFO))
	       (entry (find-entry name info)))
	  (and entry
	       (let ((file (car entry))
		     (env  (caddr entry)))
	         (and (string? file)
		      (file-exists? file)
		      (let ((saved-env (%set-global-environment env)))
		        (load file)
		        (%set-global-environment saved-env)
		        (not (null? (%env-lu name env)))
		        )))))))
    (set! remove-autoload-info
      (lambda (filename)
        (let* ((info (getprop 'PCS-AUTOLOAD-INFO 'PCS-AUTOLOAD-INFO))
	       (entry (find-entry (%system-file-name filename) info)))
	  (and entry
	       (putprop 'PCS-AUTOLOAD-INFO
			(delq! entry
			       (getprop 'PCS-AUTOLOAD-INFO
				       	'PCS-AUTOLOAD-INFO))
			'PCS-AUTOLOAD-INFO)))))
)

(define environment-bindings				; ENVIRONMENT-BINDINGS
  (letrec
   ((linked-bindings
     (lambda (a-list names values)
       (if (null? names)
	   (reverse! a-list)
	   (linked-bindings (cons (cons (car names)(cdr values))
				  a-list)
			    (cdr names)
			    (car values)))))
    (hashed-bindings
     (lambda (a-list index env)
       (if (zero? index)
	   a-list
	   (let ((bucket (%reify env index)))
	     (hashed-bindings (if (null? bucket)
				  a-list
				  (bucket-bindings a-list bucket))
			      (- index 1)
			      env)))))
    (bucket-bindings
     (lambda (a-list bucket)
       (if (null? bucket)
	   a-list
	   (bucket-bindings (cons (car bucket) a-list)
			    (cdr bucket))))))
   (lambda (obj)
     (if (null? obj)
	 obj
	 (let* ((env (cond ((environment? obj)		; environment?
			    obj)
			   ((or (closure? obj)		; closure?
				(delayed-object? obj))	; delayed object?
			    (procedure-environment obj))
			   (else
			    (%error-invalid-operand 'ENVIRONMENT-BINDINGS
						    obj))))
		(size (%reify env -1)))
	   (if (= size 12)
	       (linked-bindings '() (%reify env 1) (%reify env 2))
	       (hashed-bindings '() (- (quotient size 3) 2) env)))))))


;;;
;;; UNBIND is a function which will remove a variable's binding from a given
;;; environment. It will work for either of the 2 global environments
;;; (USER-GLOBAL-ENVIRONMENT and USER-INITIAL-ENVIRONMENT) or for any other
;;; heap allocated environments. Removing the binding from the environment
;;; will allow the garbage collector to reclaim that space. Also, once 
;;; unbound, the autoloader may reload the variable whenever that variable
;;; is referenced again.
;;;


(define unbind
  (letrec 
    ((remove-hashed-binding!		
       (lambda (key alist)
	 (cond ((null? (cadr alist))
		'()) 
	       ((eq? key (caadr alist))
		(set-cdr! alist (cddr alist)))
	       (else
		(remove-hashed-binding! key (cdr alist))))))
     
     (modify-hashed-env!
       (lambda (symbol env)	 
         (let* ((hash-val (1+ (%esc2 9 (symbol->string symbol))))
                (sym-list (%reify env hash-val)))

	   (if (null? sym-list)
	     '()
	   ;else
	     (begin
	       (if (eq? symbol (caar sym-list))
		  (set! sym-list (cdr sym-list))
	       ;else
	          (remove-hashed-binding! symbol sym-list))	
               (%reify! env hash-val sym-list)
	       env)))))

    (remove-linked-binding!
      (lambda (key names values)
	 (cond ((null? (cadr names))
		'())
	       ((eq? key (cadr names))
                (set-cdr! names (cddr names))
		(set-car! values (caar values)))
	       (else
	        (remove-linked-binding! key (cdr names) (car values)))))) 

    (modify-linked-env!
      (lambda (symbol env names values)
	(if (eq? symbol (car names))
	  (begin
	    (set! names (cdr names))
	    (set! values (car values)))
	;else
	  (remove-linked-binding! symbol names values))
        (%reify! env 1 names)
        (%reify! env 2 values)))
    )

    (lambda (symbol env)
      (cond ((not (symbol? symbol))
             (%error-invalid-operand 'UNBIND symbol))
            ((not (environment? env))
             (%error-invalid-operand 'UNBIND env))
            (else
	     (if (= (%reify env -1) 12)
               (modify-linked-env! symbol env (%reify env 1) (%reify env 2))
	     ;
	       (modify-hashed-env! symbol env)))))))


(define (procedure-environment obj)			; PROCEDURE-ENVIRONMENT
  (cond ((closure? obj)
	 (%reify obj 1))
	((delayed-object? obj)
	 (procedure-environment (vector-ref obj 1)))
	(else
	 (%error-invalid-operand 'PROCEDURE-ENVIRONMENT obj))))

