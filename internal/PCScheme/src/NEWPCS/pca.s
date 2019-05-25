
;      -*- Mode: Lisp -*-                             Filename:  pca.s

;                     Last Revision:  1-Oct-85 1700ct

;--------------------------------------------------------------------------;
;                                                                          ;
;                         TI SCHEME -- PCS Compiler                        ;
;                  Copyright 1985 (c) Texas Instruments                    ;
;                                                                          ;
;                              David Bartley                               ;
;                                                                          ;
;                  Closure Analysis and Heap Allocation                    ;
;                                                                          ;
;--------------------------------------------------------------------------;
;
;  Pass 1
;
;     Mark lambda expressions to be closed (LAMBDA-CLOSED?=T) at the point
;     of definition whenever any of the following occur:
;
;	-- the identifier bound to the lambda expression is used as a
;	   funarg					[p1-id]
;
;	-- the lambda expression is itself used as a funarg
;							[p1-lambda]
;
;	-- the identifier bound to the lambda expression is modified
;	   by SET!					[p1-set!]
;
;	-- the expression is a MULAMBDA			[p1-lambda]
;
;     Mark all identifiers which are bound to closures by LETREC:
;
;	-- ID-INIT: the lambda expression the ID was bound to
;	   (else it is NIL)				[p1-lambda]
;
;  Pass 2
;
;     Determine which variables must be heap-allocated by gathering the
;     following facts used later:
;
;	-- ID-SET!?: it is modified by a SET!		[p2-set!]
;
;	-- ID-FREEREF?: it is freely referenced by some function
;
;	-- ID-FUNARGSEES?: it is "visible" to a closed function
;
;     We do not compute the transitive closure of functions reachable from
;     closed functions.  Instead, we consider an ID to be funargref'd if
;     (1) ID is freely referenced from SOME function AND (2) ID is visible,
;     though not necessarily referenced, from a closed function.
;
;     An ID will be heap-allocated if it is potentially referenced from a
;     funarg (both ID-FREEREF? and ID-FUNARGSEES? set non-nil) and must
;     exist at runtime.  It exists at runtime if it is modified (ID-SET!?),
;     or is initialized to some value other than a lambda expression
;     (ID-INIT=NIL), or the lambda expression it is bound to is closed.
;
;--------------------------------------------------------------------------;


(define pcs-closure-analysis
  (lambda (exp)
    (letrec
;----!
     (
  (p1-exp
   (lambda (x)
     (case (car x)
       (quote            '())
       (T                (p1-id x))
       (lambda           (p1-lambda x))
       (set!             (p1-set! x))
   ;;  (if               (p1-args (cdr x)))    treat as a primop
   ;;  (begin            (p1-args (cdr x)))    treat as a primop
       (letrec           (p1-letrec x))
       (else             (p1-application x))
       )))

  (p1-id
   (lambda (id)
     (close-funarg (id-init id))))

  (p1-set!
   (lambda (x)
     (p1-id (set!-id x))
     (p1-exp (set!-exp x))))

  (p1-lambda
   (lambda (x)
     (create-lambda-label x '())
     (close-funarg x)
     (p1-exp (lambda-body x))))

  (p1-letrec
   (lambda (x)
     (let ((pairs (letrec-pairs x)))
       (p1-pairs-1 pairs)            ; link up lambda's and id's
       (p1-pairs-2 pairs)            ; find funargref's to id's
       (p1-exp (letrec-body x)))))

  (p1-pairs-1
   (lambda (pairs)
     (when pairs
	   (let* ((pr  (car pairs))
		  (id  (car pr))
		  (exp (cadr pr)))
	     (when (eq? (car exp) 'lambda)
		   (create-lambda-label exp id)
		   (set-id-init id exp)
		   (when (negative? (lambda-nargs exp))
			 (close-funarg exp)))
	     (p1-pairs-1 (cdr pairs))))))

  (p1-pairs-2
   (lambda (pairs)
     (when pairs
	   (let* ((pr  (car pairs))
		  (id  (car pr))
		  (exp (cadr pr)))
	     (if (eq? (car exp) 'lambda)
		 (p1-exp (lambda-body exp))
		 (p1-exp exp))
	     (p1-pairs-2 (cdr pairs))))))

  (p1-application
   (lambda (x)
     (let ((fn (car x))
	   (args (cdr x)))
       (p1-args args)
       (cond ((or (atom? fn)
		  (eq? (car fn) 'T))
	      '())
	     ((eq? (car fn) 'LAMBDA)
	      (p1-exp (lambda-body fn)))
	     (else
	      (p1-exp fn))))))

  (p1-args
   (lambda (args)
     (when args
	   (p1-exp (car args))
	   (p1-args (cdr args)))))

  (close-funarg
   (lambda (fn)
     (when fn
	   (set-lambda-closed? fn #!true))))

  (create-lambda-label
   (lambda (fn id)
     (set-lambda-label fn
	   (if (null? id)
	       (pcs-make-label 'lambda)
	       (cons (id-number id)(id-name id))))))

  ;;						 ------ pass 2 -------

  (p2-exp
   (lambda (x env locals)
     (case (car x)
       (quote            '())
       (T                (p2-id x env locals))
       (lambda           (p2-lambda x env locals))
       (set!             (p2-set! x env locals))
   ;;  (if               (p2-args (cdr x) env locals))    treat as a primop
   ;;  (begin            (p2-args (cdr x) env locals))    treat as a primop
       (letrec           (p2-letrec x env locals))
       (else             (p2-application x env locals))
       )))

  (p2-id
   (lambda (id env locals)
     (when (not (memq id locals))
	   (set-id-freeref? id #!true))))

  (p2-set!
   (lambda (x env locals)
     (let ((id (set!-id x))
	   (val (set!-exp x)))
       (set-id-set!? id #!true)
       (p2-id id env locals)
       (p2-exp val env locals))))

  (p2-lambda
   (lambda (x env locals)
     (let ((bvl (lambda-bvl x)))
       (when (lambda-closed? x)
	     (do ((env env (cdr env)))
		 ((null? env))
	       (do ((rib (car env)(cdr rib)))
		   ((null? rib))
		 (set-id-funargsees? (car rib) #!true))))
       (p2-exp (lambda-body x)
	       (cons bvl env)
	       bvl))))

  (p2-letrec
   (lambda (x env locals)
     (let* ((pairs  (letrec-pairs x))
	    (bvl    (mapcar car pairs))
	    (body   (letrec-body x))
	    (env    (cons bvl env))
	    (locals (append bvl locals)))
       (p2-pairs pairs env locals)
       (p2-exp body env locals))))
  
  (p2-pairs
   (lambda (pairs env locals)
     (when pairs
	   (p2-exp (cadr (car pairs)) env locals)
	   (p2-pairs (cdr pairs) env locals))))

  ;; p2-application must process IDs in function position
  ;; because they may need to be heap allocated; e.g:
  ;; (lambda (f)
  ;;   (lambda (x)   ; 'f' must be heap allocated
  ;;     (f x)))     ; 'f' appears only in function position

  (p2-application
   (lambda (x env locals)
     (let ((fn (car x)))
       (if (or (eq? fn 'THE-ENVIRONMENT)
	       (eq? fn '%MAKE-HASHED-ENVIRONMENT))
	   (smash-the-environment #!true env)
	   (let ((args (cdr x)))
	     (when (eq? fn '%CALL/CC)
		   (smash-the-environment #!false env))
	     (p2-args args env locals)
	     (when (pair? fn)
		   (if (eq? (car fn) 'LAMBDA)
		       (p2-exp (lambda-body fn)
			       (cons (lambda-bvl fn) env)
			       (lambda-bvl fn))
		       (p2-exp fn env locals))))))))

  ;; (THE-ENVIRONMENT) requires all visible lexical variables
  ;; to be heap-allocated

  (smash-the-environment
   (lambda (smash-all? env)
     (when env
	   (do ((rib (car env)			  ; CDR down this rib
		     (cdr rib)))
	       ((null? rib))
	     (let ((id (car rib))
		   (yes #!true))
	       (set-id-funargsees? id yes)
	       (set-id-freeref? id yes)
	       (when smash-all?
		     (set-id-set!? id yes)
		     (close-funarg (id-init id)))))
	   (smash-the-environment smash-all? (cdr env)))))   ; get the next rib

  (p2-args
   (lambda (args env locals)
     (when args
	   (p2-exp (car args) env locals)
	   (p2-args (cdr args) env locals))))

;----!
     )
   (begin
      (p1-exp exp)
      (p2-exp exp '() '())
      '()))))   ; executed for effect only


;==================================================================
