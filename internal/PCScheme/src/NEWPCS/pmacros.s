
;      -*- Mode: Lisp -*-			   Filename:  pmacros.s

;--------------------------------------------------------------------------;
;									   ;
;			  TI SCHEME -- PCS Compiler			   ;
;		   Copyright 1985, 1987 (c) Texas Instruments		   ;
;									   ;
;			       David Bartley				   ;
;									   ;
;			Standard Macro Definitions			   ;
;									   ;
;--------------------------------------------------------------------------;


; Revision history:
; db 10/04/85 - original
; rb 05/23/86 - treat (define var form1 ...) illegal--when "var" is a symbol,
;		there can be at most 1 form in the body
; tc  1/27/87 - Included new quasiquote expand.
; tc  2/10/87 - changed unfold-define so that MIT style define is not expanded
;		into named-lambda unless pcs-integrate-define is #T. This is
;		required for the R^3 Report.
; rb  4/ 5/87 - included XCALL macro for XLI


; runtime version of CREATE-SCHEME-MACRO is in PSTL.S
; (because this file isn't used when making runtime system)
(define create-scheme-macro			; CREATE-SCHEME-MACRO
  (lambda (name handler)
    (putprop name handler 'PCS*MACRO)
    name))

(define %expand-syntax-form                                ; %EXPAND-SYNTAX-FORM
  (lambda (form pat exp)
     (letrec
       ((compare
          (lambda (f p)
              (cond ((atom? p)
                     (cond ((symbol? p)
                            (list (cons p f)))
                           ((and (null? p) (null? f))
                            '())
                           (else (oops))))
                    ((atom? f)
                     (oops))
                    ((atom? (car p))
                     (cons (cons (car p)(car f))
                           (compare (cdr f)(cdr p))))
                    (else
                     (append! (compare (car f)(car p))
                              (compare (cdr f)(cdr p)))))))
        (substitute
          (lambda (id-list exp)
              (cond ((pair? exp)
                     (cons (substitute id-list (car exp))
                           (substitute id-list (cdr exp))))
                    ((symbol? exp)
                     (let ((x (assq exp id-list)))
                       (if (null? x)
                           exp
                           (cdr x))))
                    (else exp))))
        (oops
          (lambda ()
              (syntax-error "Invalid special form" form))))

     (substitute (compare (cdr form) pat) exp))))

(letrec
 ((csm
   (lambda (name handler)
     (putprop name handler 'PCS*MACRO)))

  (make-begin
   (lambda (x)
     (if (cdr x) (cons 'BEGIN x) (car x))))

  (unfold-define
   (lambda (form)
     (pcs-chk-length>= form form 2)
     (let ((op	 (car form))	   ; DEFINE or DEFINE-INTEGRABLE
	   (spec (cadr form))	   ; ID or (spec . bvl)
	   (body (cddr form)))	   ; rest after removing first 2 elts
       (cond ((null? body)
	      (unfold-define `(,op ,spec '#!UNASSIGNED)))
	     ((pair? spec)
	      (let ((name (car spec))
		    (bvl  (cdr spec)))
		(pcs-chk-bvl form bvl #!true)
		(unfold-define
		    (if (pair? name)
			`(,op ,name (LAMBDA ,bvl . ,body))
			(if pcs-integrate-define
			    `(,op ,name (NAMED-LAMBDA ,spec . ,body))
			    `(,op ,name (LAMBDA ,bvl . ,body))) ))))
	     (else
	      (pcs-chk-length= form form 3)
	      form)))))

  ;; EXPAND-QUASIQUOTE is adapted from an algorithm placed in
  ;; the public domain (the RRRS-Authors mailing list) on
  ;; 22-Dec-86 by Jonathan Rees of MIT.


  (expand-quasiquote
	  (lambda (x level)
	    (descend-quasiquote x level finalize-quasiquote)))

	 (finalize-quasiquote
	  (lambda (mode arg)
	    (cond ((eq? mode 'QUOTE)  `',arg)
		  ((eq? mode 'UNQUOTE) arg)
		  ((eq? mode 'UNQUOTE-SPLICING)
		   (syntax-error ",@ in illegal context" arg))
		  ((eq? mode 'UNQUOTE-SPLICING!)
		   (syntax-error ",. in illegal context" arg))
		  (else `(,mode ,@arg)))))

  (descend-quasiquote
	  (lambda (x level return)
	    (cond ((vector? x)
		   (descend-quasiquote-vector x level return))
		  ((not (pair? x))
		   (return 'QUOTE x))
		  ((eq? (car x) 'QUASIQUOTE)
		   (descend-quasiquote-pair x (+ level 1) return))
		  ((memq (car x) '(UNQUOTE UNQUOTE-SPLICING UNQUOTE-SPLICING!))
		   (if (zero? level)
		       (return (car x) (cadr x))
		       (descend-quasiquote-pair x (- level 1) return)))
		  (else
		   (descend-quasiquote-pair x level return)))))

  (descend-quasiquote-pair
	  (lambda (x level return)
	    (descend-quasiquote (car x) level		; process (car x)
	      (lambda (car-mode car-arg)
		(descend-quasiquote (cdr x) level	; process (cdr x)
		  (lambda (cdr-mode cdr-arg)
		    (cond ((and (eq? car-mode 'QUOTE)
				(eq? cdr-mode 'QUOTE))
			   (return 'QUOTE x))
			  ((eq? car-mode 'UNQUOTE-SPLICING)     ; (,@foo ...)
			   (if (and (eq? cdr-mode 'QUOTE)
				    (null? cdr-arg))
			       (return 'UNQUOTE car-arg)
			       (return 'APPEND
				       (list car-arg
					     (finalize-quasiquote
						 cdr-mode cdr-arg)))))
			  ((eq? car-mode 'UNQUOTE-SPLICING!)    ; (,.foo ...)
			   (if (and (eq? cdr-mode 'QUOTE)
				    (null? cdr-arg))
			       (return 'UNQUOTE car-arg)
			       (return 'APPEND!
				       (list car-arg
					     (finalize-quasiquote
						 cdr-mode cdr-arg)))))
			  (else
			   (return 'CONS
				   (list (finalize-quasiquote car-mode car-arg)
					 (finalize-quasiquote cdr-mode cdr-arg)
					 )))
			  )))))))

  (descend-quasiquote-vector
	  (lambda (x level return)
	    (descend-quasiquote (vector->list x) level
	      (lambda (mode arg)
		(if (eq? mode 'QUOTE)
		    (return 'QUOTE x)
		    (return 'LIST->VECTOR
			    (list (finalize-quasiquote mode arg))))))))
 )


;---- begin LETREC body ----

(csm 'access                                            ; ACCESS
  (lambda (form)
    (letrec ((help
	      (lambda (form L)
		(let ((sym (car L))
		      (env (if (null? (cddr L))   ; (access sym env)
			       (cadr L)
			       (list 'CDR (help form (cdr L))))))
		  (pcs-chk-id form sym)
		  `(%ENV-LU (QUOTE ,sym) ,env)))))
       (pcs-chk-length>= form form 2)
       (let ((id (cadr form)))
	 (pcs-chk-id form id)
	 (if (null? (cddr form))
	     id 				  ; (access id)
	     (list '%CDR
		   (help form (cdr form))))))))


(csm 'alias                                             ; ALIAS
  (lambda (form)
    (pcs-chk-length= form form 3)
    (let ((id  (cadr form))
	  (exp (caddr form)))
      (pcs-chk-id form id)
      `(CREATE-SCHEME-MACRO
	    ',id
	    (CONS 'ALIAS ',exp)))))


(csm 'and                                               ; AND
  (lambda (form)
    (pcs-chk-length>= form form 1)
    (cond ((null? (cdr form)) #!true)
	  ((null? (cddr form)) (cadr form))
	  (else `(IF ,(cadr form)
		     (AND . ,(cddr form))
		     #!FALSE)))))


(csm 'apply-if                                          ; APPLY-IF
  (lambda (form)
    (pcs-chk-length= form form 4)
    (let ((pred (cadr form))
	  (fn (caddr form))
	  (body (cadddr form)))
      `(LET ((%00000 ,pred))
	  (IF %00000 (,fn %00000)
		     ,body)))))

(csm 'assert                                            ; ASSERT
  (lambda (form)
    (pcs-chk-length>= form form 2)
    (let ((pred (cadr form))
	  (msg	(cons 'LIST (cddr form)))
	  (env	(if pcs-debug-mode '(THE-ENVIRONMENT) '())))
      `(IF ,pred
	   '()
	   (BEGIN (ASSERT-PROCEDURE ,msg ,env)
		  '())))))      ; make call non-tail-recursive

(csm 'begin0                                            ; BEGIN0
  (lambda (form)
    (pcs-chk-length>= form form 2)
    (let ((first (cadr form))
	  (rest (cddr form)))
      `(LET ((%00000 ,first))
	  (BEGIN ,@rest %00000)))))


(csm 'bkpt                                              ; BKPT
  (lambda (form)
    (pcs-chk-length= form form 3)
    `(BEGIN (BREAKPOINT-PROCEDURE ,(cadr form)
				  ,(caddr form)
				  (THE-ENVIRONMENT))
	    '())))      ; make call non-tail-recursive

(csm 'case                                              ; CASE
  (lambda (form)
    (pcs-chk-length>= form form 2)
    (let ((tag (cadr form))
	  (pairs (cddr form)))
      `(LET ((%00000 ,tag))
	  ,(let loop ((p pairs))
	      (if (null? p)
		  p
		  (let ((clause (car p)))
		    (pcs-chk-length>= clause clause 2)
		    (let ((match  (if (and (pair? (car clause))
					   (atom? (caar clause))
					   (null? (cdar clause)))
				      (caar clause)
				      (car clause)))
			  (result `(BEGIN . ,(cdr clause))))
		      (if (and (null? (cdr p))
			       (eq? match 'ELSE))
			  result
			  (let ((test (if (pair? match) 'MEMV 'EQV?)))
			    `(IF (,test %00000 ',match)
				 ,result
				 ,(loop (cdr p)))))))))))))


(csm 'cond                                              ; COND
  (lambda (form)
    (pcs-chk-length>= form form 1)
    (let ((e (cdr form)))
      (if (null? e)
	  e
	  (let ((clause (car e)))
	    (pcs-chk-length>= form clause 1)
	    (if (and (null? (cdr e))
		     (eq? (car clause) 'ELSE))  ; T handled by PME/PSIMP
		(if (null? (cdr clause))
		    #!true
		    (make-begin (cdr clause)))	 ; exp
		(let ((test (car clause))	 ; a
		      (then (cdr clause)))	 ; b
		  (if (null? (cdr e))	    ; (... (a b))
		      (if (null? then)
			  test
			  `(IF ,test ,(make-begin then) #!FALSE))
		      (if (null? then)
			  `(OR ,test
			       (COND . ,(cdr e)))
			  `(IF ,test ,(make-begin then)
			       (COND . ,(cdr e))))))))))))


(csm 'cons-stream                                       ; CONS-STREAM
  (lambda (form)
    (pcs-chk-length= form form 3)
    `(VECTOR '#!STREAM
	     ,(cadr form)
	     (%DELAY (LAMBDA () ,(caddr form))))))


(csm 'define                                            ; DEFINE
  (lambda (form)
    (unfold-define form)))


(csm 'define-integrable                                 ; DEFINE-INTEGRABLE
  (lambda (form)
    (pcs-chk-length= form form 3)
    (let* ((form (unfold-define form))
	   (id	(cadr form))
	   (exp (caddr form)))
      (pcs-chk-id form id)
      `(BEGIN
	 (PUTPROP ',id
		  (CONS 'DEFINE-INTEGRABLE ',exp)
		  'PCS*PRIMOP-HANDLER)
	 (QUOTE ,id)))))


(csm 'define-structure                                  ; DEFINE-STRUCTURE
  (lambda (form)
    (%define-structure form)))


(csm 'delay                                             ; DELAY
  (lambda (form)
    (pcs-chk-length= form form 2)
    `(VECTOR '#!DELAYED-OBJECT
	     (%DELAY (LAMBDA () ,(cadr form))))))


(csm 'do                                                ; DO
  (lambda (form)
    (letrec ((triplify
	      (lambda (old new)
		(if (atom? old)
		    (if (null? old)
			(reverse! new)
			(syntax-error "Invalid DO triples list: " form))
		    (let* ((x (car old))
			   (y (cond ((atom? x)
				     (list x '() x))
				    ((atom? (cdr x))
				     (list (car x) '() (car x)))
				    ((atom? (cddr x))
				     (list (car x)(cadr x)(car x)))
				    ((null? (cdddr x))
				     x)
				    (else (syntax-error
					      "Invalid DO list item: "
					      x)))))
		      (pcs-chk-id x (car y))
		      (triplify (cdr old)(cons y new)))))))
       (pcs-chk-length>= form form 3)
       (let* ((triples (triplify (cadr form) '()))
	      (vars    (mapcar car triples))
	      (inits   (mapcar cadr triples))
	      (steps   (mapcar caddr triples))
	      (term    (caddr form)))
	 (pcs-chk-length>= form term 1)
	 (let* ((test	 (car term))
		(body	 `(BEGIN ,@(cdddr form) (%00000 . ,steps)))
		(loop	 (if (null? (cdr term))
			     `(LET ((%00001 ,test))
				(IF %00001 %00001 ,body))
			     `(IF ,test (BEGIN . ,(cdr term)) ,body))))
	   `((REC %00000
		  (LAMBDA ,vars ,loop))
	     . ,inits))))))


(csm 'error                                             ; ERROR
  (lambda (form)
    (pcs-chk-length>= form form 2)
    (let ((msg (cadr form))
	  (irr (cond ((null? (cddr form))
		      '())
		     ((null? (cdddr form))
		      (caddr form))
		     (else
		      (cons 'LIST (cddr form)))))
	  (env (if pcs-debug-mode '(THE-ENVIRONMENT) '())))
      `(BEGIN (ERROR-PROCEDURE ,msg ,irr ,env)
	      '()))))   ; make call non-tail-recursive

(csm 'fluid                                             ; FLUID
  (lambda (form)
    (pcs-chk-length= form form 2)
    (pcs-chk-id form (cadr form))
    `(%%GET-FLUID%% (QUOTE ,(cadr form)))))


(csm 'fluid-bound?                                      ; FLUID-BOUND?
  (lambda (form)
    (pcs-chk-length= form form 2)
    (pcs-chk-id form (cadr form))
    `(%%FLUID-BOUND?%% (QUOTE ,(cadr form)))))


(csm 'fluid-lambda                                      ; FLUID-LAMBDA
  (lambda (form)
    (letrec
     ((add-bindings
       (lambda (bvl fvl body-list)
	 (if (null? bvl)
	     (cons 'BEGIN body-list)
	     (add-bindings (cdr bvl) (cdr fvl)
			   `((%%BIND-FLUID%%
			      (QUOTE ,(car fvl))
			      ,(car bvl))
			     . ,body-list))))))
     (pcs-chk-length>= form form 3)
     (pcs-chk-bvl form (cadr form) #!false)
     (if (null? (cadr form))
	 (cons 'LAMBDA (cdr form))
	 (let* ((fvl  (cadr form))
		(bvl  (mapcar (lambda (fv)(gensym '*))
			      fvl))
		(ans  (gensym '*))
		(body (cons 'BEGIN (cddr form))))
	   (list 'LAMBDA
		 bvl
		 (add-bindings
		     (reverse bvl)	; don't use REVERSE!
		     (reverse fvl)
		     `((LET ((,ans ,body))
			 (BEGIN
			   (%%UNBIND-FLUID%% ',fvl)
			   ,ans))))))))))


(csm 'fluid-let                                         ; FLUID-LET
  (lambda (form)
    (pcs-chk-length>= form form 3)
    (let ((pairs (cadr form))
	  (body (cddr form)))
      (pcs-chk-pairs form pairs)
      `((FLUID-LAMBDA ,(mapcar car pairs)
	    (BEGIN . ,body))
	. ,(mapcar cadr pairs)))))


(csm 'freeze                                            ; FREEZE
  (lambda (form)
    (pcs-chk-length>= form form 2)
    (let ((body (cdr form)))
      `(LAMBDA () . ,body))))

(csm 'inspect						; INSPECT
  (lambda (form)
    (pcs-chk-length>= form form 1)
    (let ((env (if (cdr form)
		 (begin
		   (pcs-chk-length= form form 2)
		   (cadr form))
		 '(THE-ENVIRONMENT))))
      `(begin
	 (%inspect ,env)
	 *the-non-printing-object*))))

(csm 'let                                               ; LET
  (lambda (form)
    (pcs-chk-length>= form form 3)
    (if (and (symbol? (cadr form))			; named LET
	     (not (null? (cadr form))))
	(begin
	  (pcs-chk-length>= form form 4)
	  (let ((name (cadr form))
		(pairs (caddr form))
		(body  (cdddr form)))
	    (pcs-chk-pairs form pairs)
	    `((REC ,name (LAMBDA ,(mapcar car pairs) . ,body))
	      . ,(mapcar cadr pairs))))
	(let ((pairs (cadr form))			; unnamed LET
	      (body (cddr form)))
	  (pcs-chk-pairs form pairs)
	  `((LAMBDA ,(mapcar car pairs)
	      . ,body)
	    . ,(mapcar cadr pairs))))))


(csm 'let*                                              ; LET*
  (lambda (form)
    (pcs-chk-length>= form form 3)
    (let ((pairs (cadr form))
	  (body (cddr form)))
      (if (null? pairs)
	  `(BEGIN . ,body)
	  (begin
	    (pcs-chk-pairs form pairs)
	    (let ((id (caar pairs))
		  (exp (cadar pairs)))
	      `((LAMBDA (,id)
		  (LET* ,(cdr pairs) . ,body))
		,exp)))))))


(csm 'macro                                             ; MACRO
  (lambda (form)
    (pcs-chk-length= form form 3)
    (let ((id (cadr form))
	  (fn (caddr form)))
      (pcs-chk-id form id)
      `(CREATE-SCHEME-MACRO (QUOTE ,id) ,fn))))


(csm 'make-environment                                  ; MAKE-ENVIRONMENT
  (lambda (form)
    (pcs-chk-length>= form form 1)
    `(LET ()
       ,@(cdr form)
       (THE-ENVIRONMENT))))

(csm 'make-hashed-environment                           ; MAKE-HASHED-ENVIRONMENT
  (lambda (form)
    (pcs-chk-length>= form form 1)
    `(LET ()
       (%make-hashed-environment))))

(csm 'named-lambda                                      ; NAMED-LAMBDA
  (lambda (form)
    (pcs-chk-length>= form form 3)
    (let ((bvl+ (cadr form)))
      (pcs-chk-bvl form bvl+ (not (atom? bvl+)))
      (let ((name (car bvl+))
	    (bvl  (cdr bvl+))
	    (body (cddr form)))
	`(REC ,name (LAMBDA ,bvl . ,body))))))


(csm 'or                                                ; OR
  (lambda (form)
    (pcs-chk-length>= form form 1)
    (cond ((null? (cdr form)) #!false)
	  ((null? (cddr form)) (cadr form))
	  ((or (atom? (cadr form))
	       (eq? (car (cadr form)) 'QUOTE))
	   `(IF ,(cadr form) ,(cadr form)
			     (OR . ,(cddr form))))
	  (else
	   `(LET ((%00000 ,(cadr form)))
	      (IF %00000 %00000
			 (OR . ,(cddr form))))))))


(csm 'quasiquote                                        ; QUASIQUOTE
  (lambda (form)
    (pcs-chk-length= form form 2)
    (expand-quasiquote (cadr form) 0)))


(csm 'rec                                               ; REC
  (letrec ((nice-bvl?
	    (lambda (bvl)
	      (cond ((null? bvl) #!true)
		    ((atom? bvl) #!false)
		    ((eq? (car bvl) '#!OPTIONAL) #!false)
		    (else (nice-bvl? (cdr bvl)))))))
     (lambda (form)
       (pcs-chk-length= form form 3)
       (let ((id  (cadr form))
	     (exp (caddr form)))
	 (pcs-chk-id form id)
	 (if (and (not pcs-debug-mode)
		  (pair? exp)
		  (eq? (car exp) 'LAMBDA)
		  (pair? (cdr exp))
		  (nice-bvl? (cadr exp)))
	     (let ((bvl (cadr exp)))
	       `(LETREC ((,id ,exp))
		     (LAMBDA ,bvl (,id . ,bvl))))
	     `(LETREC ((,id ,exp)) ,id))))))


(csm 'sequence                                          ; SEQUENCE
  (lambda (form)
    (pcs-chk-length>= form form 1)
    (cons 'BEGIN (cdr form))))


(csm 'set-fluid!                                        ; SET-FLUID!
  (lambda (form)
    (pcs-chk-length= form form 3)
    (pcs-chk-id form (cadr form))
    `(%%SET-FLUID%% (QUOTE ,(cadr form))
		    ,(caddr form))))


(csm 'set!                                              ; SET!
  (lambda (form)
    (pcs-chk-length= form form 3)
    (let ((spec (cadr form))
	  (value (caddr form)))
      (if (pair? spec)
	  (let ((op (car spec)))
	    (case op
	      ((ACCESS)
		  (pcs-chk-length>= spec spec 2)
		  (let ((sym (cadr spec))
			(env (cond ((null? (cddr spec))
				    '(THE-ENVIRONMENT))
				   ((null? (cdddr spec))
				    (caddr spec))
				   (else
				    `(ACCESS . ,(cddr spec))))))
		    (pcs-chk-id spec sym)

		    `(LET ((%00000 ,env))
			(%DEFINE ',sym ,value %00000)
			'())

;;;		    `(LET* ((%00000    ; do this first, since it
;;;				,env)  ; may be (THE-ENVIRONMENT)
;;;			    (%00001 ,value)
;;;			    (%00002 (%SET-GLOBAL-ENVIRONMENT %00000)))
;;;		       (%%DEF-GLOBAL%% ',sym %00001)
;;;		       (%SET-GLOBAL-ENVIRONMENT %00002)
;;;		       '())

			   ))
	      ((FLUID)
		  (pcs-chk-length= spec spec 2)
		  (pcs-chk-id spec (cadr spec))
		  `(SET-FLUID! ,(cadr spec) ,value))
	      ((VECTOR-REF)
		  (pcs-chk-length= spec spec 3)
		  `(VECTOR-SET! ,(cadr spec) ,(caddr spec) ,value))
	      (else
	       (let ((mac (getprop op 'PCS*MACRO)))
		 (if (null? mac)
		     (let ((g (getprop op 'PCS*PRIMOP-HANDLER)))
		       (if (and (pair? g)
				(eq? (car g) 'DEFINE-INTEGRABLE)
				(pair? (cdr g))
				(eq? (cadr g) 'LAMBDA)
				(pair? (cddr g))
				(pair? (cdddr g))
				(null? (cddddr g)))
			   (let ((args (caddr g))
				 (body (cadddr g)))
			     `((LAMBDA ,args (SET! ,body ,value))
			       ,@(cdr spec)))
			   form))
		     `(SET! ,(if (pair? mac)
				 (cons (cdr mac)(cdr spec))  ; alias
				 (mac spec))		     ; macro
			    ,value))))))
	  form))))


(csm 'syntax                                            ; SYNTAX
  (lambda (form)
    (pcs-chk-length= form form 3)
    (let ((pat (cadr form))
	  (exp (caddr form)))
      (if (and (pair? pat)(symbol? (car pat)))
	  `(CREATE-SCHEME-MACRO
	      ',(car pat)       ; macro name
	      (LAMBDA (FORM)
		(%EXPAND-SYNTAX-FORM FORM ',(cdr pat) ',exp)))
	  (syntax-error "Invalid SYNTAX form: " form)))))


(csm 'unassigned?                                       ; UNASSIGNED?
  (lambda (form)
    (pcs-chk-length= form form 2)
    (let ((sym (cadr form)))
      (pcs-chk-id form sym)
      `(EQ? ,sym '#!UNASSIGNED))))


(csm 'unbound?                                          ; UNBOUND?
  (lambda (form)
    (pcs-chk-length>= form form 2)
    (let ((sym (cadr form))
	  (env (cond ((null? (cddr form))
		      (list 'THE-ENVIRONMENT))
		     ((null? (cdddr form))
		      (caddr form))
		     (else
		      (cons 'ACCESS (cddr form))))))
      (pcs-chk-id form sym)
      `(NULL? (%ENV-LU (QUOTE ,sym) ,env)))))

(csm 'xcall                                             ; XCALL (for XLI)
  (lambda (e)
    (pcs-chk-length>= e e 2)
    (let ((fn (cadr e))
	  (args (cddr e)))
    `(%xesc ,(+ (length args) 2) ,fn ,@args))))


(csm 'when                                              ; WHEN
  (lambda (form)
    (pcs-chk-length>= form form 3)
    (let ((pred (cadr form))
	  (body (cons 'BEGIN (cddr form))))
      `(IF ,pred ,body '()))))

'()
) ;---- end LETREC body ----
