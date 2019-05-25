
;      -*- Mode: Lisp -*-			    Filename:  pinspect.s

;                     Last Revision:  12-Nov-85 1400ct

;--------------------------------------------------------------------------;
;                                                                          ;
;                         TI SCHEME -- PCS Compiler                        ;
;                  Copyright 1985 (c) Texas Instruments                    ;
;                                                                          ;
;                              David Bartley                               ;
;                                                                          ;
;                   The Inspector and %PCS-EDIT-BINDING                    ;
;                                                                          ;
;--------------------------------------------------------------------------;


(define %inspect					; %INSPECT
  (lambda (cur-env)
    (cond ((environment? cur-env)
	   (%inspector '() '() '()
		       cur-env
		       (%reify-stack (+ (%reify-stack -1) 6))
		       0))
	  ((closure? cur-env)
	   (%inspect (procedure-environment cur-env)))
	  (else
	   (display "Invalid operand to INSPECT: ")
	   (display cur-env)))))


(define %inspector					; %inspector
  (letrec
   ((table
     '((1 . "All")					; ctrl-A
       (2 . "Backtrace calls")				; ctrl-B
       (3 . "Current environment frame")		; ctrl-C
       (4 . "Down to callee")				; ctrl-D
       (5 . "Edit: ")					; ctrl-E
       (7 . "Go")					; ctrl-G
       (9 . "Inspect: ")				; ctrl-I
       (12 . "List Procedure")				; ctrl-L
       (13 . "Repeat Breakpoint Message")		; ctrl-M
       (16 . "`Parent' environment frame")		; ctrl-P
       (17 . "Quit")					; ctrl-Q
       (18 . "Return with the value: ")			; ctrl-R
       (19 . "`Son' environment frame")			; ctrl-S
       (21 . "Up to caller")				; ctrl-U
       (22 . "Value of: ")				; ctrl-V
       (23 . "Where am I?")				; ctrl-W
       (#\SPACE . "Value of: ")
       (#\! . "Reinitialize INSPECT!")
       (#\? . "?")))

    (repl
     (lambda ()
       (pcs-clear-registers)
       (fresh-line)
       (newline)
       (display "[Inspect] ")
       (flush-input)
       (let* ((ch (read-char))
	      (key (if (memv ch '(#\SPACE #\! #\?))
		       ch
		       (char->integer ch)))
	      (entry (assv key table)))
	 (when entry
	       (display (cdr entry)))
	 (case key
	   (1   (all cur-env 0)(repl))			; ctrl-A
	   (2   (newline)(where stk-index)		; ctrl-B
		(backtrace stk-index)(repl))
	   (3   (newline)				; ctrl-C
		(current cur-env 0 #!true)
		(repl))
	   (4   (newline)				; ctrl-D
		(down)(repl))
	   (5   (let ((ans				; ctrl-E
		       (%pcs-edit-binding '() (read) cur-env)))
		  (when (string? ans)(display ans))
		  (repl)))
	   ((7 18)					; ctrl-G, ctrl-R
	        (leave key))
	   (12  (newline)				; ctrl-L
		(pp (%reify-stack (+ stk-index 15)))
		(repl))
	   (13  (newline)				; ctrl-M
		(display kind)
		(when kind
		      (when msg (display msg))
		      (newline)
		      (write irritant))
		(repl))
	   (16  (newline)				; ctrl-P
		(parent cur-env)(repl))
	   (17  (reset))				; ctrl-Q
	   (19  (newline)				; ctrl-S
		(son)(repl))
	   (21  (newline)				; ctrl-U
		(up)(repl))
	   ((22 #\SPACE)
	        (pp (eval (read) cur-env))		; ctrl-V, SPACE
		(repl))
	   (23  (newline)				; ctrl-W
		(where stk-index)(repl))
	   (#\!   (newline)(init)(repl))		;  !
	   (#\?   (newline)				;  ?
		  (help)(repl))
	   (else
	    (if (eqv? key 9)				; ctrl-I
		(let ((env (eval (read) cur-env)))
		  (cond ((or (environment? env)
			     (closure? env)
			     (delayed-object? env))
			 (set! (fluid %inspector-continuation) '())
			 (%inspect env))
			(else
			 (display (integer->char 7))	; beep
			 (display "  ?  Not an environment: ")
			 (write env)))
		  (repl))
		(begin
		  (display (integer->char 7))		; beep
		  (display "  ?  Invalid response...  Type `?' for help")
		  (repl))))))
       ))

    (All
     (lambda (env depth)
       (fresh-line)
       (when (and env (not (eq? env user-global-environment)))
	     (current env depth #!true)
	     (all (environment-parent env) (+ depth 1)))))

    (Backtrace
     (lambda (stk-index)
       (let ((si (%reify-stack (+ stk-index 6))))
	 (fresh-line)
	 (when (positive? si)
	       (display "  called from   ")
	       (display (%reify-stack (+ si 15)))
	       (backtrace si)))))

    (Current
     (lambda (env depth verbose?)
       (when verbose?
	     (display "Environment frame bindings at level ")
	     (display (+ depth (length son-stk)))
	     (cond ((eq? env user-initial-environment)
		    (display "  (USER-INITIAL-ENVIRONMENT)"))
		   ((eq? env user-global-environment)
		    (display "  (USER-GLOBAL-ENVIRONMENT)"))))
       (when (or verbose?
		 (=? (%reify env -1) 12))	; not a global environment
	     (let ((frame (environment-bindings env)))
	       (if (null? frame)
		   (begin
		     (newline)
		     (display "    --no variables--"))
		   (let loop ((pairs frame))
		     (when pairs
			   (newline)
			   (display "    ")
			   (if (char-ready?)
			       (display "[aborted]")
			       (let ((val (cdar pairs)))
				 (display (caar pairs)) 	; var
				 (display " ")
				 (tab27 (current-column))
				 (cond ((pair? val)
					(display "-- list --"))
				       ((vector? val)
					(display "-- vector --"))
				       (else (write val)))
				 (loop (cdr pairs))))))
		   )))))

    (Down
     (lambda ()
       (if (null? down-stk)
	   (display "  ?  Can't move Down")
	   (let ((si (car down-stk)))
	     (set! down-stk (cdr down-stk))
	     (set! stk-index si)
	     (set! son-stk '())
	     (set! cur-env (%reify-stack (+ si 9)))
	     (where si)))))

    (Leave
     (lambda (key)
       (cond ((not (zero? exit-code))
	      (newline)
	      (display "  ?  Sorry, the program is not resumable")
	      (repl))
	     ((eqv? key 7)				; ctrl-G
	      (newline)
	      '())
	     ((memq msg '(BREAK-ENTRY BREAK-EXIT))
	      ((fluid %*BREAK*continuation) (eval (read) cur-env)))
	     (else
	      (newline)
	      (display "  ?  Sorry, use `ctrl-R' only to return from BREAK")
	      (repl)))))

    (Parent
     (lambda (env)
       (let ((penv (environment-parent env)))
	 (if (null? penv)
	     (display "  ?  No parent exists")
	     (begin
	       (set! son-stk (cons env son-stk))
	       (set! cur-env penv)
	       (current penv 0 #!true))))))

    (Son
     (lambda ()
       (if (null? son-stk)
	   (display "  ?  No son exists")
	   (begin
	     (set! cur-env (car son-stk))
	     (set! son-stk (cdr son-stk))
	     (current cur-env 0 #!true)))))

    (Up
     (lambda ()
       (let ((si (%reify-stack (+ stk-index 6))))
	 (if (positive? si)
	     (begin
	       (set! down-stk (cons stk-index down-stk))
	       (set! son-stk '())
	       (set! cur-env (%reify-stack (+ si 9)))
	       (set! stk-index si)
	       (where si))
	     (display "  ?  Can't move Up")))))

    (Where
     (lambda (si)
       (display "Stack frame for ")
       (display (%reify-stack (+ si 15)))
       (current cur-env 0 #!false) ))

    (tab27
     (lambda (cur)
       (cond ((>? 24 cur) (display "   ")(tab27 (+ cur 3)))
	     ((>? 27 cur) (display " ")  (tab27 (+ cur 1)))
	     ((= 27 cur)  cur)
	     (else        (newline)    (tab27 1)))))

    (init
     (lambda ()
       (set! son-stk '())
       (set! down-stk '())
       (set! cur-env orig-env)
       (set! stk-index orig-stk-index) ))

    (help
     (lambda ()
       (mapc (lambda (x)(display x))
	 '("   ?    -- display this command summary" #\newline
	   "   !    -- reinitialize INSPECT" #\newline
	   " ctrl-A -- display All environment frame bindings" #\newline
	   " ctrl-B -- display procedure call Backtrace" #\newline
	   " ctrl-C -- display Current environment frame bindings" #\newline
	   " ctrl-D -- move Down to callee's stack frame" #\newline
	   " ctrl-E -- Edit variable binding" #\newline
	   " ctrl-G -- Go  (resume execution)" #\newline
	   " ctrl-I -- evaluate one expression and Inspect the result"
	   #\newline
	   " ctrl-L -- List current procedure" #\newline
	   " ctrl-M -- repeat the breakpoint Message" #\newline
	   " ctrl-P -- move to Parent environment's frame" #\newline
	   " ctrl-Q -- Quit  (RESET to top level)" #\newline
	   " ctrl-R -- Return from BREAK with a value" #\newline
	   " ctrl-S -- move to Son environment's frame" #\newline
	   " ctrl-U -- move Up to caller's stack frame" #\newline
	   " ctrl-V -- eValuate one expression in current environment"
	   #\newline
	   " ctrl-W -- (Where) Display current stack frame" #\newline
	   "To enter `ctrl-A', press both `CTRL' and `A'."
           ))))

    ;; data

    (down-stk '())
    (son-stk '())
    (orig-env '())
    (orig-stk-index '())
    (msg '())
    (kind '())
    (irritant '())
    (cur-env '())
    (stk-index '())
    (exit-code '())
    )
   (lambda (msg0 kind0 irritant0 cur-env0 stk-index0 exit-code0)
     (if (and (fluid-bound? %inspector-continuation)
	      (not (null? (fluid %inspector-continuation))))
	 ((fluid %inspector-continuation) '())
	 (fluid-let ((%inspector-continuation '()))
	   (set! msg msg0)
	   (set! kind kind0)
	   (set! irritant irritant0)
	   (set! cur-env cur-env0)
	   (set! stk-index stk-index0)
	   (set! exit-code exit-code0)
	   (set! orig-env cur-env0)
	   (set! orig-stk-index stk-index0)
	   (init)
	   (call/cc
	      (lambda (k)
		(set! (fluid %inspector-continuation) k)))
	   (repl)))
     )))



;;; %PCS-EDIT-BINDING
;;;
;;;   argument OBJ:       () or value to be edited
;;;   optional arg NAME:  symbol
;;;   optional arg ENV:   environment for name
;;;
;;;   When NAME and ENV are not supplied, %PCS-EDIT-BINDING calls the
;;;   editor to edit OBJ.
;;;
;;;   When NAME and ENV are supplied, %PCS-EDIT-BINDING calls the editor
;;;   to create a new binding for the name in the environment.  If OBJ is
;;;   nil, the current binding of NAME in ENV is edited instead of OBJ.
;;;
;;;   returns either (1) an error message string or
;;;		     (2) (LIST edited-value)

(define %pcs-edit-binding
  (letrec ((help
	    (lambda (obj name)
	      (if (closure? obj)
		  (let ((info (assq 'SOURCE (%reify obj 0))))
		    (if (null? info)
			"[No source found for compiled procedure.]"
			(let ((new (edit (cdr info))))
			  (if (and (pair? new)
				   (eq? (car new) 'LAMBDA))
			      (let ((mode pcs-debug-mode))
				(set! pcs-debug-mode #!true)
				(let ((value (eval new)))
				  (set! pcs-debug-mode mode)
				  (%reify! value 0
					  (cons (cons 'SOURCE new) name))
				  (list value)))
			      (list new)))))
		  (list (edit obj))))))
     (lambda (obj . rebind)
       (if (null? rebind)
	   (help obj rebind)
	   (let ((name (car rebind))
		 (env  (cadr rebind)))
	     (if (and (symbol? name)(environment? env))
		 (let ((value-list (help (or obj (cdr (%env-lu name env)))
				    name)))
		   (if (atom? value-list)
		       value
		       (let ((value (car value-list))
			     (cell (%env-lu name env)))
			 (if (null? cell)
			     (%define name value env)
			     (set-cdr! cell value)))))
		 "[Invalid argument]"))))))
