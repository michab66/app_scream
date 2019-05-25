;      -*- Mode: Lisp -*-			      Filename:  pcomp.s

;--------------------------------------------------------------------------;
;									   ;
;			  TI SCHEME -- PCS Compiler			   ;
;		   Copyright 1985 (c) Texas Instruments 		   ;
;									   ;
;			       Terry Caudill				   ;
;									   ;
;		   Compiler Specific runtime routines   		   ;
;									   ;
;--------------------------------------------------------------------------;

; Revision history:
;  6/01/87 tc - This file was created from several of the old compiler
;               files (pstd, pio, pdebug, pchreq, and pstl) in order to
;		collect all the compiler-specific code.
;  6/01/87 tc - added PCS-INTEGRATE-DEFINE variable so that MIT style
;		defines don't expand into named-lambda unless #T. This
;		is a requirement for the R^3 Report.
;  6/01/87 tc - added string->number as autoload from PNUM2S
;  6/01/87 tc - make compiler re-entrant
;  6/01/87 rb - added more PGR functions to autoload;
;		toplevel reworked so RESET doesn't affect the fluids
;		INPUT-PORT and OUTPUT-PORT (this allows the system toplevel
;		to run in windows other than 'CONSOLE);
;		revamped PCS-INITIAL-ARGUMENTS per 3.0 changes to cmd line
;  6/01/87 tc - added MAKE-STRING as autoload for PFUNARG

;;;
;;; The following functions are related in that they all envoke the
;;; compiler in some form or fashion
;;;
(define load						; LOAD
       (lambda (filename)
	 (let ((i-port (open-input-file filename)))
           (if (null? i-port)
               (error "Unable to load file" filename)
               (letrec
                 ((loop
                    (lambda (form)
                      (cond ((eof-object? form)
                             (close-input-port i-port)
                             'ok)
                            (else
                              (eval form)
                              (loop (read i-port)))))))
                 (let ((form (read i-port)))
                   (if (eq? form '#!fast-load)
                       (begin
                         (close-input-port i-port)
                         (fast-load filename))
                       (loop form))))))))

(define compile-file					; COMPILE-FILE
       (lambda (filename1 filename2)
	 (if (or (not (string? filename1))
		 (not (string? filename2))
		 (equal? filename1 filename2))
	     (%error-invalid-operand-list 'COMPILE-FILE
		    filename1
		    filename2)
	     (let ((i-port (open-input-file filename1)))
	       (let ((o-port (open-output-file filename2)))
		 (set-line-length! 74 o-port)
		 (letrec
		   ((loop
			(lambda (form)
			  (if (eof-object? form)
			      (begin (close-input-port i-port)
				     (close-output-port o-port)
				     'ok)
			      (begin			; no COMPILE-FORMS
				 (compile-to-file form)
				 (set! form '())     ; for GC
				 (loop (read i-port))))))
		    (compile-to-file
			(lambda (form)
			  (let ((cform (compile form)))
			    (write (list '%execute (list 'quote cform))
				   o-port)
			    (newline o-port)
			    (%execute cform)))))
		 (loop (read i-port))))))))

(define %compile-timings '())

(define %compile					; %COMPILE
  (lambda (exp . time?)
    (when time? (gc))
    (let ((time '())
          (t0 (runtime)))
      (set! pcs-local-var-count 0)
      (set! pcs-error-flag #!false)
      (set! pcs-verbose-flag (not time?))
      (set! pcs-binary-output #!false)
      (set! pme= (pcs-macro-expand exp))
      (if pcs-error-flag
          (error "[Compilation terminated because of errors]")
          (begin
            (set! time (cons (- (runtime) t0) time))
            (set! psimp= (pcs-simplify pme=))
            (set! time (cons (- (runtime) t0) time))
            (pcs-closure-analysis psimp=)
            (set! time (cons (- (runtime) t0) time))
            (set! pcg= (pcs-gencode psimp=))
            (set! time (cons (- (runtime) t0) time))
            (set! ppeep= (pcs-postgen pcg=))
            (set! time (cons (- (runtime) t0) time))
            (set! pasm= (pcs-assembler ppeep=))
            (set! time (cons (- (runtime) t0) time))
            (set! pcs-verbose-flag #!false)
            (when time?
                  (set! %compile-timings
                        (cons (reverse! time) %compile-timings)))
            pasm=)))))

;
; Make compiler re-entrant (or more so, at any rate). The problem arises
; when a macro evokes EVAL and thus COMPILE during macro expansion i9n PME
;
(define compile '())                                    ; COMPILE

(let ((ge (%set-global-environment user-global-environment)))
      (set! compile
	(lambda (exp)
	  (let* ((vc pcs-local-var-count)	; save
		 (vf pcs-verbose-flag)
		 (ef pcs-error-flag)
		 (bo pcs-binary-output)
		 (gensym-string (access string (procedure-environment gensym)))
		 (gensym-counter (access counter (procedure-environment gensym)))
		 (result (pcs-assembler (pcs-compile-to-AL exp))))
	    (set! pcs-local-var-count vc)	; restore
	    (set! pcs-verbose-flag vf)
	    (set! pcs-error-flag ef)
	    (set! pcs-binary-output bo)
	    (set! (access string (procedure-environment gensym)) gensym-string)
	    (set! (access counter (procedure-environment gensym)) gensym-counter)
	    (pcs-clear-registers)
	    result)))
      (%set-global-environment ge))

(define pcs-compile-to-AL				; PCS-COMPILE-TO-AL
  (lambda (exp)
    (set! pcs-local-var-count 0)
    (set! pcs-error-flag #!false)
    (set! pcs-binary-output #!true)
    (set! pcs-verbose-flag #!false)
    (let ((t1 (pcs-macro-expand exp)))
      (if pcs-error-flag
          (error "[Compilation terminated because of errors]")
          (begin
            (set! exp '())                          ; for GC
            (pcs-clear-registers)
            (let ((t2 (pcs-simplify t1)))
              (pcs-closure-analysis t2)
              (let ((t3 (pcs-gencode t2)))
                (set! t2 '())                               ; for GC
                (pcs-clear-registers)
                (let ((t4 (pcs-postgen t3)))
                  (pcs-clear-registers)
                  t4))))))))

(define pcs-execute-AL					; PCS-EXECUTE-AL
  (lambda (al)
    (let ((t1 (pcs-assembler al)))
      (pcs-clear-registers)
      (%execute t1))))

(define optimize!					; OPTIMIZE!
  (lambda args
    (let ((flag (or (null? args)(car args))))
      (set! pcs-permit-peep-1 flag)
      (set! pcs-permit-peep-2 flag))))


;;;; Syntax Checking Functions
;;;
;;; These functions may be used by macros and other syntax transformers
;;; to help find violations of Scheme syntax rules.  Note that these
;;; check only the syntax, not semantics, of the program fragments they
;;; are defined for.  It is the caller's responsibility, for example, to
;;; verify that all of the identifiers bound in a LETREC are distinct.
;;; PCS-CHK-PAIRS can't do so, because it is called to verify pairs for
;;; both LETREC and LET*.

(define pcs-chk-id					; PCS-CHK-ID
  (lambda (e y)
    (when (not (symbol? y))
          (syntax-error "Invalid identifier in expression" y e))))

(define (pcs-chk-length= e y n)				; PCS-CHK-LENGTH=
  (cond ((and (null? y)(zero? n))
         '())
        ((null? y)
         (syntax-error "Expression has too few subexpressions" e))
        ((atom? y)
         (syntax-error (if (atom? e)
                           "List expected"
                           "Expression ends with `dotted' atom")
                       e))
        ((zero? n)
         (syntax-error "Expression has too many subexpressions" e))
        (else
          (pcs-chk-length= e (cdr y) (sub1 n)))))

(define (pcs-chk-length>= e y n)			; PCS-CHK-LENGTH>=
  (cond ((and (null? y)( < n 1))
         '())
        ((atom? y)
         (pcs-chk-length= e y -1))
        (else
          (pcs-chk-length>= e (cdr y) (sub1 n)))))

(define (pcs-chk-bvl e bvl dot-ok?) 		; PCS-CHK-BVL
  (letrec ((oops
             (lambda () (syntax-error "Invalid identifier list" e))))
    (cond ((atom? bvl)
           (or (null? bvl)(and dot-ok? (pcs-chk-bvar bvl))
               (oops)))
          ((pcs-chk-bvar (car bvl))
           (pcs-chk-bvl e (cdr bvl) dot-ok?))
          (else
            (oops)))))

(define (pcs-chk-pairs e pairs)				; PCS-CHK-PAIRS
  (letrec ((oops
             (lambda () (syntax-error "Invalid pair binding list" e))))
    (if (atom? pairs)
        (or (null? pairs)
            (oops))
        (let ((pr (car pairs)))
          (if (or (atom? pr)
                  (not (pcs-chk-bvar (car pr)))
                  (atom? (cdr pr))
                  (not (null? (cddr pr))))
              (oops)
              (pcs-chk-pairs e (cdr pairs)))))))


(define pcs-chk-bvar					; PCS-CHK-BVAR
  (lambda (id)
    (if (or (not (symbol? id))
            (getprop id 'PCS*MACRO)
            (memq id '(QUOTE LAMBDA IF SET!
                             BEGIN LETREC DEFINE))
            (and (memq id '(T NIL))
                 pcs-integrate-t-and-nil))
        (syntax-error "Invalid bound variable name" id)
        #!true)))

;;; EXPAND, EXPAND-MACRO and EXPAND-MACRO-1 expand macro calls. EXPAND-MACRO
;;; and EXPAND-MACRO-1 only expand the outer-level form and leave sub-forms 
;;; alone.  EXPAND-MACRO-1 does so only once, while EXPAND-MACRO does so 
;;; repeatedly until there is no change. EXPAND expands form and all subforms
;;; completely.

(define expand-macro					; EXPAND-MACRO
  (lambda (exp)
    (let ((expansion (expand-macro-1 exp)))
      (if (or (atom? exp) (equal? expansion exp))
	  expansion
	  (expand-macro expansion)))))

(define expand-macro-1					; EXPAND-MACRO-1
  (lambda (x)
    (cond ((symbol? x)
	   (let ((entry (getprop x 'PCS*MACRO)))
	     (if (null? entry)
                 x
                 (if (pair? entry)
                     (if (eq? (car entry) 'ALIAS)
                         (cdr entry))
                     (syntax-error "Macro or special form name used as a variable"
                                   x)))))
	  ((pair? x)
	   (let* ((f  (car x))
		  (ef (if (pair? f) (expand-macro f) f))
		  (a  (cdr x)))
	     (if (symbol? ef)
                 (let ((macfun (getprop ef 'PCS*MACRO)))
                   (cond ((null? macfun)
                          (cons ef a))
                         ((pair? macfun)
                          (cons (cdr macfun) a))
                         (else
                           (macfun (cons ef a)))))
                 (cons ef a))))
	  (else x))))

(define expand						; EXPAND
  (letrec ((expand-item
             (lambda (item)
               (if (pair? item) (expand item) item))))
    (lambda (exp)
      (let ((expansion (expand-macro exp)))
        (map expand-item expansion)))))

;;;
;;; Set up EDWIN so that it may be loaded into its own environment
;;;

(define initiate-edwin					; INITIATE-EDWIN
  (lambda ()
    (unbind 'edwin user-global-environment)
    (set! (access edwin-environment user-global-environment)
	  (make-hashed-environment))
    (%reify! edwin-environment 0 user-initial-environment)
    (autoload-from-file (%system-file-name "edwin0.fsl")
      '(edwin)
      edwin-environment)
    (edwin)))

(define edwin initiate-edwin)				; EDWIN

;;;
;;; Set up compiler-related global variables
;;;

(BEGIN
 (define %pcs-stl-debug-flag #!false)
 (define %pcs-stl-history    '(%PCS-STL-HISTORY))  ; getprop tag
 (define pcs-local-var-count	   0)
 (define pcs-integrate-integrables  #!true)
 (define pcs-integrate-primitives   #!true)
 (define pcs-integrate-T-and-NIL    #!true)
 (define pcs-integrate-define	   #!true)
 (define pcs-debug-mode		   #!false)	; debug mode OFF
 (define pcs-permit-peep-1	   #!true)	; optimization ON
 (define pcs-permit-peep-2	   #!true)
 (define pcs-verbose-flag	   #!false)
 (define pcs-display-warnings	   #!true)
 (define pme=   '())
 (define psimp= '())
 (define pcg=   '())
 (define ppeep= '())
 (define pasm=  '())
)

;;; Evaluation

;;; EVAL is part interpreter, but calls the compiler for complicated
;;; expressions.  In particular, it does not do any bindings
;;; interpretively, since they would have to be first-class
;;; environments and the compiler might be able to do better.

(define eval
  (letrec
    ((eval-exp
       (lambda (xx env)
         (let ((x (expand-macro xx)))
           (if (pair? x)
               (case (car x)
                 ((QUOTE)		(eval-quote x env))
                 ((IF) 		(eval-if x env))
                 ((SET!)		(eval-set! x env))
                 ((DEFINE)		(eval-define x env))
                 ((BEGIN)		(eval-begin x env))
                 ((LET
		    LET*
		    LETREC
		    LAMBDA )		(eval-compile x env))
                 ((%%GET-FLUID%%)	(eval-fluid x env))
                 ((%%SET-FLUID%%)	(eval-set-fluid! x env))
                 ((THE-ENVIRONMENT)	env)
                 ((PCS-CODE-BLOCK)	(eval-execute x env))
                 (else 		(eval-application x env)))
               (eval-atom x env)))))
     
     (lookup-binding					; LOOKUP-BINDING
       (lambda (sym)
         ; The following is the object code to lookup/fetch
         ; the binding of sym. It must be passed to %execute with
         ; the desired environment.
         (list 'pcs-code-block 1 4 (list sym)
               '( 7 4 0       ; Ld-global r1,sym
                    59))))       ; exit
     
     (eval-atom					; EVAL-ATOM
       (lambda (x env)
         (cond ((not (symbol? x))			  x)
               ((memq x '(#!TRUE #!FALSE #!UNASSIGNED))  x)
               (else
		 (let ((entry (and PCS-INTEGRATE-T-AND-NIL
				   (assq x '((T  #T) (NIL  #F))))))
		   (if entry
                       (cadr entry)
                       ;else
                       (or (lookup-integrable x env)
                           (eval-execute (lookup-binding x) env))))))))
     
     (lookup-integrable
       (lambda (x env)
         (let ((info (getprop x 'PCS*PRIMOP-HANDLER)))
           (and info
                (pair? info)
                (eval-exp (cdr info) env)))))
     
     (eval-quote					; EVAL-QUOTE
       (lambda (x env)
         (pcs-chk-length= x x 2)
         (cadr x)))
     
     (eval-id-error
       (lambda (err caller env)
         (syntax-error
           (string-append "Invalid identifier for " caller ": ") err)))
     
     
     (eval-if 					; EVAL-IF
       (lambda (x env)
         (if (or (atom? (cdr x))	; No Pred
                 (atom? (cddr x))	; No Then
                 (pair? (cdddr x)))	; has ELSE
	     (pcs-chk-length= x x 4)
	     (pcs-chk-length= x x 3))
         (cond ((eval-exp (cadr x) env)
                (eval-exp (caddr x) env))
               ((pair? (cdddr x))
                (eval-exp (cadddr x) env))
               (else
		 #!FALSE))))
     
     
     (set-var-value					; SET-VAR-VALUE
       (lambda (sym value)
         ; The following is the object code code to set the value
         ; of a variable. It must be passed to %execute with the
         ; desired environment.
         (list 'pcs-code-block 2 7 (list sym value)
               '( 1 4 1        ; Load r1, value
                    15 4 0	; St-glob-env r1,sym
                    59))))	; exit
     
     (eval-set!					; EVAL-SET!
       (lambda (x env)
         (pcs-chk-length= x x 3)
         (let* ((id	(cadr x))
                (var	(expand-macro id))
                (value (eval-exp (caddr x) env)))
           (cond ((not (pair? var))
                  (cond ((or (not (symbol? var))
                             (not (eq? var (expand-macro var))))
                         (eval-id-error var "SET!" env))
                        ((getprop var 'PCS*PRIMOP-HANDLER)
                         ; this is for primitives and define-integrables
                         (eval-compile x env))
                        (else
			  (eval-execute (SET-VAR-VALUE var value) env))))
                 (else
		   (eval-id-error var "SET!" env))))))
     
     (def-var   					; DEF-VAR
       (lambda (sym value)
         ; The following is the object code code to define a variable
         ; in a given environment. It must be passed to %execute with the
         ; desired environment.
         (list 'pcs-code-block 2 7 (list sym value)
               '( 1  4 1        ; Load r1, value
                  31 4 0	; define!  value,sym
                  59))))	; exit
     
     (eval-define					; EVAL-DEFINE
       (lambda (x env)
         (pcs-chk-length>= x x 3)
         (if (and (pair? (caddr x))
                  (memq (caaddr x) '(LAMBDA NAMED-LAMBDA)))
             (eval-compile x env)
             ;else
             (let* ((id	  (cadr x))
                    (var   (expand-macro id))
                    (value (eval-exp (caddr x) env)))
               (cond ((not (pair? var))
                      (cond ((or (not (symbol? var))
                                 (not (eq? var (expand-macro var))))
                             (eval-id-error var "DEFINE" env))
                            ((getprop var 'PCS*PRIMOP-HANDLER)
                             ; this is for primitives and define-integrables
                             (eval-compile x env))
                            (else
			      (eval-execute (DEF-VAR var value) env)
                              id)))
                     (else
                       (eval-id-error var "DEFINE" env)))))))
     
     
     (eval-begin					; EVAL-BEGIN
       (lambda (x env)
         (pcs-chk-length>= x x 1)
         (let loop ((x (cdr x)))
           (if (null? (cdr x))
               (eval-exp (car x) env)
               (begin
                 (eval-exp (car x) env)
                 (loop (cdr x)))))))
     
     (lookup-fluid					; LOOKUP-FLUID
       (lambda (sym)
         ; The following is the object code to lookup/fetch the
         ; fluid binding of sym. It must be passed to %execute with
         ; the desired environment.
         (list 'pcs-code-block 1 4 (list sym)
               '( 8 4 0       ; Ld_fl r1,sym
                    59))))       ; exit
     
     (eval-fluid					; EVAL-FLUID
       (lambda (x env)
         (pcs-chk-length= x x 2)
         (eval-execute (lookup-fluid (eval-exp (cadr x) env)) env)))
     
     (set-fluid-var					; SET-FLUID-VAR
       (lambda (sym value)
         ; The following is the object code to set the value of a
         ; fluid variable. It must be passed to %execute with the
         ; desired environment.
         (list 'pcs-code-block 2 7 (list sym value)
               '( 1 4 1        ; Load  r1, value
                    16 4 0	; St-fl r1,sym
                    59))))	; exit
     
     (eval-set-fluid! 				; EVAL-SET-FLUID!
       (lambda (x env)
         (pcs-chk-length>= x x 2)
         (let ((sym  (eval-exp (cadr x) env))
               (val (eval-exp (caddr x) env)))
           (pcs-chk-id x sym)
           (eval-execute (set-fluid-var sym val) env))))
     
     (eval-application				; EVAL-APPLICATION
       (lambda (x env)
         (pcs-chk-length>= x x 1)
         (let ((proc (eval-exp (car x) env)))
           (when (not (or (procedure? proc)
                          (and (pair? proc)
                               (eq? (car proc) 'LAMBDA))))
                 (error-procedure "Attempt to call a non-procedural object"
                                  (cons proc (cdr x))
                                  env))
           (let ((args (eval-args (cdr x) env)))
             (let* ((saved-env (%set-global-environment env))
                    (result (apply proc args)))
               (%set-global-environment saved-env)
               result)))))
     
     (eval-args					; EVAL-ARGS
       (lambda (x env)
         (if (null? x)
             '()
             (cons (eval-exp  (car x) env)
                   (eval-args (cdr x) env)))))
     
     (eval-compile					; EVAL-COMPILE
       (lambda (x env)
         (eval-execute (compile x) env)))
     
     (eval-execute					; EVAL-EXECUTE
       (lambda (x env)
         (let* ((saved-env (%set-global-environment env))
                (result (%execute x)))
           (%set-global-environment saved-env)
           result)))
     
     ) ; letrec vars
    
    (lambda (exp . rest)
      (let* ((env (cond ((null? rest)
                         (let ((e (%set-global-environment
                                    user-initial-environment)))
                           (%set-global-environment e)
                           e))
			((not (environment? (car rest)))
			 (%error-invalid-operand 'EVAL (car rest)))
			(else
                          (car rest))))
	     (result (eval-exp exp env)))
        result))))
       
