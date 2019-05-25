
;      -*- Mode: Lisp -*-                          Filename:  psimp.s

;                     Last Revision:  1-Oct-85 1630ct

;--------------------------------------------------------------------------;
;                                                                          ;
;                         TI SCHEME -- PCS Compiler                        ;
;                  Copyright 1985 (c) Texas Instruments                    ;
;                                                                          ;
;                              David Bartley                               ;
;                                                                          ;
;                         Program Simplification                           ;
;                  (for use only after alpha conversion)                   ;
;                                                                          ;
;--------------------------------------------------------------------------;


(define pcs-simplify
  (lambda (exp)
    (letrec
;-------!
 ((simp
   (lambda (x)
     (if (atom? x)
	 x
	 (case (car x)
	   (quote           x)
	   (T               x)				; ID record
	   (lambda          (simp-lambda x))
	   (if              (simp-if (simp (if-pred x))
				     (simp (if-then x))
				     (simp (if-else x))))
	   (set!            (simp-set! (set!-id x)
				       (simp (set!-exp x))))
	   (begin           (simp-begin (simp-args (cdr x) '())))
	   (letrec          (simp-letrec
			     (simp-pairs (letrec-pairs x) '())
			     (simp (letrec-body x))))
	   (else            (simp-application (simp-args x '())))
	   ))))

  (simp-lambda
   (lambda (x)    ; note: preserve extra slots in the node
     (begin       ;    This changes the apparent output of PME!!
       (set-lambda-body x (simp (lambda-body x)))
       x)))

  (simp-if
   (lambda (p th el)
     (cond		          ;; --- (if p (if p a b) c) ==> (if p a c)

           ((and (eq? (car th) 'if)
		 (dupe? p)			; no side effects
		 (equal? p (if-pred th)))
	    (simp-if p (if-then th) el))

				  ;; --- (if p a (if p b c)) ==> (if p a c)

	   ((and (eq? (car el) 'if)
		 (dupe? p)			; no side effects
		 (equal? p (if-pred el)))
	    (simp-if p th (if-else el)))

			          ;; --- (if #!false a b) ==> b
			          ;; --- (if '* a b) ==> a

	   ((eq? (car p) 'quote)
	    (if (cadr p) th el))

			          ;; --- (if (not a) b c) ==> (if a c b)

	   ((eq? (car p) 'not)
	    (simp-if (cadr p) el th))

			          ;; --- (if (begin ... p) a b)
			          ;; ==> (begin ... (if p a b))

	   ((eq? (car p) 'begin)
	    (let ((sl (reverse (cdr p))))
	      (simp-begin
	       (reverse! (cons (simp-if (car sl) th el)
			       (cdr sl))))))

			          ;; --- (if (if a b c) d e)
			          ;;
			          ;; ==> (if a (if b d e)
			          ;;           (if c d e))

	   ((eq? (car p) 'if)
	    (cond ((dupe? th)
		   (let ((a (if-pred p))
			 (b (if-then p))
			 (c (if-else p)))
		     (cond
			          ;; --- (if (if a 't c) d e)
				  ;; ==> (if a d (if c d e))

		          ((and (pair? b)
				(eq? (car b) 'QUOTE)
				(cadr b))
			   (simp-if a th
				      (simp-if c th el)))

			          ;; --- (if (if a b 't) d e)
				  ;; ==> (if a (if b d e) d)

		          ((and (pair? c)
				(eq? (car c) 'QUOTE)
				(cadr c))
			   (simp-if a (simp-if b th el) th))

			          ;; --- (if (if a a c) d e)
				  ;; ==> (if a d (if c d e))

			  ((and (dupe? a)(equal? a b))
			   (simp-if a th (simp-if c th el)))

			  (else
			   (list 'if p th el)))))

	;; The following turns out to "pessimize" the code
	;; given the current code generator algorithms

	;;	  ((dupe? el)
	;;	   (let ((a (if-pred p))
	;;		 (b (if-then p))
	;;		 (c (if-else p)))
	;;	     (cond
			          ;; --- (if (if a #!false c) d e)
				  ;; ==> (if a e (if c d e))

	;;		  ((equal? b '(quote #!false))
	;;		   (simp-if a el (simp-if c th el)))

			          ;; --- (if (if a b #!false) d e)
				  ;; ==> (if a (if b d e) e)

	;;		  ((equal? c '(quote #!false))
	;;		   (simp-if a (simp-if b th el) el))
	;;		  (else
	;;	  	   (list 'if p th el)))))

		  (else
		   (list 'if p th el))))

	   (else
	    (list 'if p th el)))))

  (dupe?
   (lambda (x)
     (or (atom? x)
	 (memq (car x)
	       '(T  QUOTE  %%get-global%%  %%get-fluid%%)))))

  (simp-set!
   (lambda (id exp)
     (cond
			          ;; --- (set! a a) ==> a

           ((eq? id exp) id)

			          ;; --- (set! x (if a b c))
			          ;; ==> (if a (set! x b)(set! x c))

	   ((eq? (car exp) 'if)
	    (simp-if (if-pred exp)
		     (simp-set! id (if-then exp))
		     (simp-set! id (if-else exp))))
	   (else
	    (list 'set! id exp)))))

  (simp-begin
   (lambda (sl)
     (let ((sl (s-begin (reverse! sl) '())))
       (cond ((null? sl) '(quote ()))
	     ((null? (cdr sl)) (car sl))
	     (else
	      (cons 'begin sl))))))

  (s-begin
   (lambda (old new)
     (if (null? old)
	 new
	 (let ((s (car old)))
	   (cond ((and new				; not last exp
		       (memq (car s) 
			     '(T QUOTE LAMBDA
			       %%get-global%%
			       %%get-fluid%%)))
		  (s-begin (cdr old) new))		; delete s
		 ((or (eq? (car s) 'begin)
		      (and new (no-se-op (car s))))
		  (s-begin (append! (reverse! (cdr s))
				    (cdr old))
			   new))
		 (t (s-begin (cdr old)
			     (cons s new))))))))

;;;  (simp-apply
;;;   (lambda (fun arg)
;;;     (cond
;;;	 		       ;; --- (apply (lambda (a ...) body) arg)
;;;			       ;; ==> (let ((L arg))
;;;			       ;;       (let ((a (car L))...) body))
;;;
;;;        ((and (eq? (car fun) 'lambda)
;;;		 (not (negative? (lambda-nargs fun))))
;;;	    (simp-apply-letrec
;;;	        (lambda-bvl fun) (lambda-body fun) arg #!false))
;;;
;;;	   (t (list '%apply fun arg)))))

;;;(simp-apply-letrec
;;;(lambda (bvl body arg dupe?)
;;;				;; (apply (lambda () body) L)
;;;				;; ==> (begin L body)
;;;  (if (null? bvl)
;;;	 (simp-begin (list arg body))
;;;	 (let ((a (car bvl)))
;;;	   (cond
;;;				;; (apply (lambda (a ...) body) (cons x y))
;;;				;; ==> (let ((a x))
;;;				;;       (apply (lambda (...) body) y))
;;;	       ((eq? (car arg) 'cons)
;;;		(simp-letrec
;;;		    `((,a ,(cadr arg)))
;;;		    (simp-apply-letrec
;;;		        (cdr bvl) body (caddr arg) #!false)))
;;;
;;;				;; (apply (lambda (a) body) L)
;;;				;; ==> (let ((a (car L))) body)
;;;	       ((null? (cdr bvl))
;;;		(simp-letrec
;;;		    `((,a (car ,arg)))
;;;		       body))
;;;				;; (apply (lambda (a...) body) triv)
;;;				;; ==> (let ((a (car triv)))
;;;				;;       (apply (lambda (...) body)
;;;				;;              (cdr triv)))
;;;	       ((or dupe?
;;;		    (memq (car arg) '(T QUOTE)))
;;;		(simp-letrec
;;;		    `((,a (car ,arg)))
;;;		       (simp-apply-letrec
;;;			   (cdr bvl) body `(cdr ,arg) 't)))
;;;
;;;				;; (apply (lambda (a...) body) L)
;;;				;; ==> (let ((temp L))
;;;				;;       (let ((a (car L)))
;;;				;;         (apply (lambda (...) body)
;;;				;;                (cdr temp))))
;;;	       (t
;;;		(let ((temp (pcs-make-id '())))
;;;		  (simp-letrec
;;;		      `((,temp ,arg))
;;;		         (simp-letrec
;;;			     `((,a (car ,temp)))
;;;			        (simp-apply-letrec
;;;				    (cdr bvl) body `(cdr ,temp) 't)))))
;;;	       )))))

  (simp-letrec
   (lambda (pairs body)
     (cond
			       ;; --- (letrec () body) ==> body

	   ((and (null? pairs)
		 (not debug-mode))
	    body)

			       ;; --- (letrec ((a '*)...)
			       ;;         (begin (set! a value) ...))
			       ;; --- (letrec (...(a value))
			       ;;         (begin ...))

;;; omit: works, but not worth doing
;;;	   ((and (eq? (car body) 'begin)
;;;		 (eq? (car (cadr body)) 'set!)
;;;		 (eq? (set!-id (cadr body)) (caar pairs))
;;;		 (eq? (car (cadar pairs)) 'quote)
;;;		 (memq (car (set!-exp (cadr body)))
;;;		       '(quote  lambda)))
;;;	    (simp-letrec
;;;		(append (cdr pairs)
;;;			(list
;;;			     (list (caar pairs)
;;;				   (set!-exp (cadr body)))))
;;;		(simp-begin
;;;		    (cddr body))))

			       ;; --- (letrec ((a '*)...)
			       ;;         (set! a value))
			       ;; --- (letrec (...(a value))
			       ;;         a)

;;; omit: works, but not worth doing
;;;	   ((and (eq? (car body) 'set!)
;;;		 (eq? (set!-id body) (caar pairs))
;;;		 (eq? (car (cadar pairs)) 'quote)
;;;		 (memq (car (set!-exp body))
;;;			'(quote  lambda)))
;;;	    (simp-letrec
;;;		(append! (cdr pairs)
;;;			 (list
;;;			     (list (set!-id body)
;;;				   (set!-exp body))))
;;;		(set!-id body)))

	   (t (list 'letrec pairs body)))))

  (simp-pairs
   (lambda (old new)
     (if (null? old)
	 (reverse! new)
	 (simp-pairs (cdr old)
		     (cons (list (caar old)
				 (simp (cadar old)))
			   new)))))

  (simp-car
   (lambda (x)
     (if (atom? x)
	 (list 'CAR x)
	 (let ((op (assq (car x) '((CAR . CAAR)(CADR . CAADR)
				   (CDR . CADR)(CDDR . CADDR)
				   (CDDDR . CADDDR)))))
	   (if op
	       (cons (cdr op)(cdr x))
	       (list 'CAR x))))))

  (simp-cdr
   (lambda (x)
     (if (atom? x)
	 (list 'CDR x)
	 (let ((op (assq (car x) '((CAR . CDAR)(CADR . CDADR)
				   (CDR . CDDR)(CDDR . CDDDR)))))
	   (if op
	       (cons (cdr op)(cdr x))
	       (list 'CDR x))))))

  (simp-=
   (lambda (op x y)
     (if (and (eq? (car y) 'QUOTE)
	      (number? (cadr y)))
	 (let ((rop (assq op '((=  . =)  (<  . >)  (>  . <)
			       (<= . >=) (>= . <=) (<> . <>)))))
	   (if rop
	       (list (cdr rop) y x)
	       (list op x y)))
	 (list op x y))))

  (simp-application
   (lambda (comb)			; COMB is already simplified
     (let ((f (car comb))
	   (args (cdr comb)))
       (cond ((atom? f)				; primitive
	      (case f
;;;		((%apply)  (simp-apply (car args) (cadr args)))
		((car)     (simp-car (car args)))
		((cdr)     (simp-cdr (car args)))
		((= < >  <= >= <>)
		           (simp-= f (car args) (cadr args)))
		(else
		 comb)))

				    ;; --- ((lambda () body)) ==> body

	     ((and (not debug-mode)
		   (eq? (car f) 'lambda)
		   (null? args)
		   (null? (lambda-bvl f)))
	      (lambda-body f))

				    ;; --- ((lambda (a b)(foo a b))
				    ;;      x y)
				    ;; ==> (foo x y)

	     ((and (not debug-mode)
		   (eq? (car f) 'lambda)
		   (let ((foo (car (lambda-body f))))
		     (cond ((atom? foo)
			    (getprop foo 'pcs*opcode))
			   ((eq? (car foo) 'T)
			    (not (memq foo (lambda-bvl f))))
			   (else
			    (eq? (car foo) '%%get-global%%))))
		   (equal? (cdr (lambda-body f))  ; (... a b)
			   (lambda-bvl f)))	     ; (a b)
	      (simp-application
	          (cons (car (lambda-body f))
			args)))

				    ;; --- ((letrec pairs body) . args)
				    ;; ==> (letrec pairs (body . args))

	     ((eq? (car f) 'letrec)
	      (simp-letrec
	          (letrec-pairs f)
		  (simp-application
		      `(,(letrec-body f) . ,args))))

	     (t comb)))))

  (simp-args
   (lambda (old new)
     (if (null? old)
	 (reverse! new)
	 (simp-args (cdr old)
		    (cons (simp (car old))
			  new)))))

  (no-se-op
   (lambda (op)
     (and (symbol? op)
	  (getprop op 'pcs*primop-handler)	; not a 'magic' primop
	  (let ((opcode (getprop op 'pcs*opcode)))
	    (and (integer? opcode)
		 (positive? opcode))))))

;;; data

  (debug-mode pcs-debug-mode)

;-------!
	)
 (simp exp))))
