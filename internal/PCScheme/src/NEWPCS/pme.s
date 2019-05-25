
;      -*- Mode: Lisp -*-                             Filename:  pme.s

;                     Last Revision: 1-Oct-85 1400ct

;--------------------------------------------------------------------------;
;                                                                          ;
;                         TI SCHEME -- PCS Compiler                        ;
;                  Copyright 1985 (c) Texas Instruments                    ;
;                                                                          ;
;                              David Bartley                               ;
;                                                                          ;
;                  Macro Expansion and Alpha Conversion                    ;
;                                                                          ;
;--------------------------------------------------------------------------;
;
;   Alpha conversion technique:
;
;	All lexical identifiers (not global or fluid variables) are changed
;	to "id records" organized as shown:
;
;	     (T  (original-name . unique-number)  .  <flags>)
;
;	The tag "T" is used because it does not conflict with any valid
;	names for primitive operations.  The "unique-number" is for human
;	consumption but may also be used to create an assembler label.
;
;	Global and fluid variables are not considered in the "core".  The
;	following primitive functions are used to manipulate them:
;
;		(%%get-global%%   (quote symbol))
;		(%%set-global%%   (quote symbol) exp)
;		(%%def-global%%   (quote symbol) exp)
;
;		(%%get-fluid%%    (quote symbol))
;		(%%set-fluid%%    (quote symbol) exp)
;
;		(%%bind-fluid%%   (quote symbol) exp)
;		(%%unbind-fluid%% (quote (symbol ...)))
;		(%%fluid-bound?%% (quote symbol))
;
;	Names of official SCHEME 84 primitive functions are not considered
;	to be global variables.  When used in the function position of a
;	combination, they are left as atoms.  Funarg uses of such
;	primitives are changed to dummy closures:
;
;             (foo eq?)  ==>  (foo (lambda (a b) (eq? a b)))
;
;   Node annotation:
;
;	Lambda and mulambda nodes are extended with extra "slots" for use
;	during closure analysis as follows.  Mulambda's are represented by
;	a negative argument count and a "normalized" argument list.
;
;            (lambda  bvl  body  nargs  label  debug  closed?)
;
;--------------------------------------------------------------------------;




(define pcs-macro-expand				; PCS-MACRO-EXPAND
  (lambda (exp)
    (letrec
;------!
     (
  (chk-id        (lambda (e y) (pcs-chk-id e y)))	; syntax checkers
  (chk-length=   (lambda (e y n) (pcs-chk-length= e y n)))
  (chk-length>=  (lambda (e y n) (pcs-chk-length>= e y n)))
  (chk-bvl       (lambda (a b c) (pcs-chk-bvl a b c)))
  (chk-pairs     (lambda (a b) (pcs-chk-pairs a b)))

  (expand
   (lambda (x env)
     (cond ((atom? x)
	    (exp-atom x env))
	   ((macro? (car x))
	    (exp-macro x env))
	   (else
	    (expand2 x env)))))

  (exp-macro
   (lambda (x env)
     (let ((y (if (pair? macfun)
		  (cons (cdr macfun)(cdr x))	; alias
		  (macfun x))))			; macro
       (if (or (atom? y)
	       (equal? x y))
	   (expand2 y env)
	   (expand y env)))))

  (macfun '())

  (macro?
   (lambda (id)
     (set! macfun
	   (and (symbol? id)
		(or (getprop id 'PCS*MACRO))))
     macfun))

  (expand2
   (lambda (x env)
     (if (atom? x)
	 (exp-atom x env)
	 (case (car x)
	   (quote           (exp-quote x))
	   (lambda          (exp-lambda x env))
	   (if              (exp-if x env))
	   (set!            (exp-set! x env))
	   (define          (exp-define x env))
	   (begin           (exp-begin x env))
	   (letrec          (exp-letrec x env))
	   (not             (exp-not x env))
	   (else            (exp-application x env))
	   ))))

  (exp-quote
   (lambda (x)
     (chk-length= x x 2)
     x))

  (exp-atom
   (lambda (x env)
     (let ((info (assq x '((T . '#!TRUE)(NIL . '())))))
       (cond (info
	      (if integrate-T-and-NIL?
		  (cdr info)
		  (lookup x env)))
	     ((or (null? x)
		  (not (symbol? x))
		  (memq x '(#!TRUE #!FALSE #!UNASSIGNED)))
	      (list 'QUOTE x))
	     (else
	      (lookup x env))))))

  (exp-lambda
   (lambda (x env)
     (chk-length>= x x 3)
     (let ((bvl (lambda-bvl x)))
       (chk-bvl x bvl #!true)
       (let ((node (help-lambda bvl
			(make-contour (lambda-body-list x) env '())
			'()  0  env)))
	 (let ((name (fluid name)))	; guess at closure name
	   (set-lambda-debug node
	       (if pcs-debug-mode
		   (cons (cons 'SOURCE x) name)
		   name)))
	 node))))

  (make-contour
   (lambda (sl env pairs)
     (if (or (null? sl)
	     (atom? (car sl)))
	 (make-letrec sl env pairs)
	 (let* ((s (car sl))
		(op (car s)))
	   (if (macro? op)
	       (let* ((y (if (pair? macfun)
			     (cons (cdr macfun)(cdr s))	; alias
			     (macfun s)))		; macro
		      (sl (cons y (cdr sl))))
		 (if (equal? s y)
		     (help-contour sl env pairs)	; exit loop
		     (make-contour sl env pairs)))	; repeat loop
	       (help-contour sl env pairs))))))

  (help-contour
   (lambda (sl env pairs)
     (let ((s (car sl)))
       (case (car s)
	 (DEFINE
	   (let* ((name (cadr s))
		  (exp  (caddr s))
		  (pair (if (and (symbol? name)
				 (pair? exp)
				 (eq? (car exp) 'NAMED-LAMBDA)
				 (pair? (cdr exp))
				 (pair? (cadr exp))
				 (eq? (car (cadr exp)) name))
			    (let ((bvl (cdr (cadr exp)))
				  (bdy (cddr exp)))
			      `(,name (LAMBDA ,bvl . ,bdy)))
			    (cdr s))))
	     (make-contour (cdr sl) env (cons pair pairs))))
	 (BEGIN
	  (make-contour (append (cdr s)(cdr sl)) env pairs))
	 (else
	  (make-letrec sl env pairs))))))

  (make-letrec
   (lambda (sl env pairs)
     (if (null? pairs)
	 (make-body sl)
	 `(LETREC ,(reverse! pairs) . ,sl))))

  (help-lambda
   (lambda (old-bvl body new-bvl nargs env)
     (cond ((null? old-bvl)
	    (let* ((bvl (reverse! new-bvl))
		   (env (extend env bvl)))
	      (pcs-extend-lambda
		  (list 'LAMBDA
			(mapcar (lambda (id) (cdr (assq id env)))
				bvl)
			(expand body env)
			nargs))))
	   ((atom? old-bvl)				; mulambda
	    (help-lambda '()
			 body
			 (cons old-bvl new-bvl)
			 (minus (add1 nargs))
			 env))
	   (else
	    (help-lambda (cdr old-bvl)
			 body
			 (cons (car old-bvl) new-bvl)
			 (add1 nargs)
			 env)))))

;   Below, perform the optimization
;
;        (if (or a b) x y)  ===>  (if (and (not a)(not b)) y x)
;
;   which allows the AND macro to generate better code.

  (exp-if
   (lambda (x env)
     (if (or (atom? (cdr x))(atom? (cddr x))(atom? (cdddr x)))
	 (chk-length= x x 3)
	 (chk-length= x x 4))
     (let ((pred (if-pred x))
	   (then (if-then x))
	   (else (if (null? (cdddr x))
		     ''()
		     (if-else x))))
       (if (and (not (atom? pred))
		(eq? (car pred) 'OR))
	   (list 'IF
		 (expand (cons 'AND
			       (mapcar (lambda (arg) (list 'NOT arg))
				       (cdr pred)))
			 env)
		 (expand else env)
		 (expand then env))
	   (list 'IF
		 (expand pred env)
		 (expand then env)
		 (expand else env))))))

;   Below, perform the optimization
;
;        (not (or a b))  ===>  (and (not a)(not b))
;
;   which allows the AND macro to generate better code.

  (exp-not
   (lambda (x env)
     (chk-length= x x 2)
     (if (and (primitive? 'NOT env)
	      (pair? (cadr x))
	      (eq? (car (cadr x)) 'OR))
	 (expand
	    (cons 'AND (mapcar (lambda (opd) (list 'NOT opd))
			       (cdr (cadr x))))
	    env)
	 (exp-application x env))))

  (exp-set!
   (lambda (x env)
     (chk-length= x x 3)
     (let* ((id  (set!-id x))
	    (var (lookup-LHS id "SET!" env))
	    (val (fluid-let ((name id))
		     (expand (set!-exp x) env))))
       (if (atom? var)
	   `(%%SET-GLOBAL%% (QUOTE ,var) ,val)
	   `(SET! ,var ,val)))))

  (exp-define
   (lambda (x env)
     (chk-length>= x x 3)
     (let* ((id  (set!-id x))
	    (var (lookup-LHS id "DEFINE" env))
	    (val (fluid-let ((name id))
		    (expand (set!-exp x) env))))
       (when (not (null? env))
	     (syntax-error "Incorrectly placed DEFINE" x))
       (if (atom? var)
	   `(%%DEF-GLOBAL%% (QUOTE ,id) ,val)	; global
	   `(BEGIN (SET! ,var ,val)		; lexical
		   (QUOTE ,id))))))

  (exp-begin
   (lambda (x env)
     (chk-length>= x x 1)
     (make-body (mapcar (lambda (s) (expand s env))
			(help-begin (cdr x) '())))))

;   Below, perform the optimization
;
;     (begin ... (or a ...) ...)  ==>  (begin ... (and (not a)...) ...)
;
;   which allows the AND macro to generate better code.

  (help-begin
   (lambda (old new)
     (if (null? old)
	 (reverse! new)
	 (help-begin
	     (cdr old)
	     (cons
	        (let ((s (car old)))
		  (if (and (cdr old)		; leave last stmt alone
			   (not (atom? s))
			   (eq? (car s) 'OR))
		      (cons 'AND
			    (mapcar (lambda (a) (list 'NOT a))
				    (cdr s)))
		      s))
		new)))))

  (exp-letrec
   (lambda (x env)
     (chk-length>= x x 3)
     (chk-pairs x (letrec-pairs x))
     (let ((env  (extend env (mapcar car (letrec-pairs x))))
	   (body (make-contour (letrec-body-list x) env '())))
       (list 'LETREC
	     (exp-pairs (letrec-pairs x) '() env)
	     (expand body env)))))

  (exp-pairs
   (lambda (old new env)
     (if (null? old)
	 (reverse! new)
	 (let ((id  (cdr (assq (caar old) env)))
	       (exp (fluid-let ((name (caar old)))
			 (expand (cadar old) env))))
	   (exp-pairs (cdr old)
		      (cons (list id exp) new)
		      env)))))

  (exp-application
   (lambda (form env)
     (chk-length>= form form 1)
     (let ((fn   (car form))
	   (args (cdr form)))
       (cond ((pair? fn)
	      (let* ((exp (exp-args form '() env))
		     (xfn (car exp)))
		(cond ((or (atom? xfn)
			   (not (eq? (car xfn) 'LAMBDA)))
		       exp)
		      ((negative? (lambda-nargs xfn))
		       (let ((id (pcs-make-id 'MULAMBDA))) ; must guarantee
			 `(LETREC ((,id ,xfn))	        ;  no "mulambda" in
			     (,id . ,(cdr exp)))))      ; "function position"
		      ((=? (length args)(lambda-nargs xfn))
		       exp)
		      (else
		       (syntax-error "Wrong number of arguments" form)))))
	     ((symbol? fn)
	      (let ((lex (assq fn env)))
		(if lex
		    (cons (cdr lex)(exp-args args '() env))
		    (apply-if
		       (lookup-primop fn integrate-global?
				         integrate-primitive?)
		       (lambda (info)
			 (cond ((integer? info)
				(chk-length= form (cdr form) info)
				(cons fn (exp-args (cdr form) '() env)))
			       ((pair? info)
				    ;; integrable definition
				(exp-integrable form (cdr info) (cdr form)
						env))
			       (else
				    ;; VM primitive
				(let ((form2 (info form)))
				  (if (equal? form form2)
				      (cons (car form)
					    (exp-args
					     (cdr form) '() env))
				      (expand form2 env))))))
		       (cons (make-global fn)
			     (exp-args args '() env))))))
	     (else
	      (syntax-error "Invalid function name" fn))))))

  (exp-args
   (lambda (old new env)
     (if (null? old)
	 (reverse! new)
	 (exp-args (cdr old)
		   (cons (expand (car old) env) new)
		   env))))

  (exp-integrable
   (lambda (form fn args env)
     (letrec ((mismatch
	       (lambda (x y)
		 (cond ((null? x)  (not (null? y)))
		       ((atom? x)  #!false)
		       ((atom? y)  #!true)
		       (else       (mismatch (cdr x)(cdr y)))))))
       (if (and (pair? fn)
		(eq? (car fn) 'LAMBDA)
		(pair? (cdr fn))
		(mismatch (cadr fn) args))
	   (syntax-error "Wrong number of arguments" form)
	   (expand (cons fn args) env)))))

  (make-body
   (lambda (lst)
     (cond ((null? lst)       ''())
	   ((null? (cdr lst)) (car lst))
	   (else              (cons 'BEGIN lst)))))

  (extend
   (lambda (env bvl)
     ;; note: error checking done earlier
     (cond (bvl
	    (let* ((var (car bvl))
		   (new (pcs-make-id var))
		   (rib (cons var new)))
	      (extend (cons rib env)
		      (cdr bvl))))
	   (env
	    env)
	   (else	; distinguish `empty env' from `no env'
	    '((()))))))

  (lookup
   (lambda (id env)
     (apply-if (getprop id 'PCS*MACRO)
	 (lambda (mac)
	   (if (pair? mac)
	       (expand (cdr mac) env)		; alias
	       (syntax-error			; macro
		  "Macro name used as variable" id)))
	 (apply-if (assq id env)
	     (lambda (lex) (cdr lex))		; lexical var
	     (let ((info (lookup-primop id
					integrate-global?
					integrate-primitive?)))
	       (cond ((or (null? info)
			  (integer? info))
		      (make-global id))
		     ((pair? info)
		      (expand (cdr info) env))
		     (else
		      (expand (info id) env))))))))

  (lookup-LHS
   (lambda (id caller env)
     (if (or (null? id)
	     (not (symbol? id))
	     (getprop id 'PCS*MACRO))	; macro or alias
	 (syntax-error (string-append "Invalid identifier for " caller ": ") id)
	 (let ((lex (assq id env)))
	   (cond (lex (cdr lex))
		 ((and display-warnings?
		       (lookup-primop id integrate-global?
				         integrate-primitive?))
		  (writeln
		      "[WARNING: modifying an `integrable' variable: "
		      id "]")
		  id)
		 (else id))))))

  (lookup-primop
   (lambda (id integrate-global? integrate-primitive?)
     (and (symbol? id)
	  (let ((info (getprop id 'PCS*PRIMOP-HANDLER)))
	    (and info
		 (if (pair? info) integrate-global? integrate-primitive?)
		 info)))))

  (primitive?
   (lambda (id env)
     (and (not (getprop id 'PCS*MACRO))
	  (not (assq id env))
	  (let ((info (lookup-primop id #!false integrate-primitive?)))
	    (or (integer? info)
		(closure? info))))))

  (make-global
   (lambda (id)
     `(%%GET-GLOBAL%% (QUOTE ,id))))

  ;;; data

  (integrate-global?      pcs-integrate-integrables)
  (integrate-primitive?   pcs-integrate-primitives)
  (integrate-T-and-NIL?   pcs-integrate-T-and-NIL)
  (display-warnings?	  pcs-display-warnings)

;------!
       )

    (fluid-let ((name '()))	; default lambda "name"
	(expand exp '())))))
