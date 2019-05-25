
;      -*- Mode: Lisp -*-                             Filename:  pgencode.s

;                     Last Revision:  1-Oct-85 1630ct

;--------------------------------------------------------------------------;
;                                                                          ;
;                         TI SCHEME -- PCS Compiler                        ;
;                  Copyright 1985 (c) Texas Instruments                    ;
;                                                                          ;
;                              David Bartley                               ;
;                                                                          ;
;                             Code Generation                              ;
;                                                                          ;
;--------------------------------------------------------------------------;
;
;   Note:  The current implementation never changes REG-BASE, so the
;          registers may be sparsely used.  Consider using fewer registers
;          and implementing a wrap-around algorithm.
;
;   Note:  There is currently no check to ensure that DEST never exceeds
;          MAX-REGNUM.  Somebody ought to do something about that!
;	   (Implementing wrap-around would fix this, too.)
;
;--------------------------------------------------------------------------;

(define pcs-gencode
  (lambda (exp)
    (letrec
;------!
       ((debug-mode	      pcs-debug-mode) 

	(max-regnum           62)    ; highest available register number
						  ; r0 reserved for '()
						  ; r63 used by ppeep
	(compiled-lambda-list '())   ; code for previously compiled closures

	(gen-code
	    (lambda (entry-name ; label for the code block
		     body       ; expression to be compiled
		     bvl        ; bound variable list
		     lex-level  ; lambda nesting level
		     senv       ; stack component of the lexical environment
		     henv       ; heap component of the lexical environment
		     cenv)      ; compile-time component of the lex env
	      (letrec
;--------------!
	       (
  (code       '())    ; list of generated instructions and labels
  (tos         -1)    ; stack level (size of current frame)
  (reg-base    -1)    ; stack offset equivalent to register 0
  (last-label '())    ; last code entry label referenced

  (gen
      (lambda (x dest tr?)
	(cond ((atom? (car x))
	       (case (car x)
		 (quote        (gen-quote x dest tr?))
		 (T            (gen-id x dest tr?))
		 (lambda       (gen-closure x dest tr?))
		 (if           (gen-if x dest tr?))
		 (set!         (gen-set! x dest tr?))
		 (%call/cc     (gen-ccc x dest tr?))
		 (begin        (gen-begin (cdr x) dest tr?))
		 (%apply       (gen-apply x dest tr?))
		 (letrec       (gen-letrec x dest tr?))
		 (else         (gen-primitive x dest tr?))))
	      ((eq? (caar x) 'LAMBDA)
	       (gen-let x dest tr?))
	      (else
	       (gen-application x dest tr?)))))

  (gen-quote
      (lambda (x dest tr?)
	(emit-load dest
		   (if (null? (cadr x)) 0 x))		; use R0 for '()
	(continue dest tr?)))

  (gen-id
      (lambda (id dest tr?)
	(let ((name (id-name id))
	      (info (assq id senv)))
	  (if info
	      (let ((dlevel (- lex-level (cddr info)))
		    (offset (cadr info)))
		(if (and (zero? dlevel) ( > offset tos))
		    (emit-load dest (- offset reg-base) name)
		    (emit-load dest `(STACK ,offset ,dlevel) name)))
	      (emit-load dest (list 'HEAP name)))
	  (continue dest tr?))))

  (gen-set!
      (lambda (x dest tr?)
	(let* ((id (cadr x))
	       (value (caddr x))
	       (name (id-name id))
	       (info (assq id senv)))
	  (gen value dest #!false)
	  (if info
	      (let ((dlevel (- lex-level (cddr info)))
		    (offset (cadr info)))
		(if (and (zero? dlevel) ( > offset tos))
		    (emit-load (- offset reg-base) dest (cons 'SET name))
		    (emit 'STORE `(STACK ,offset ,dlevel) dest name)))
	      (emit 'STORE (list 'HEAP name) dest))
	  (continue dest tr?))))

  (gen-closure
      (lambda (x dest tr?)
	(let ((label (lambda-label x))
	      (bvl   (lambda-bvl x)))
	  (gen-code label
		    (lambda-body x)
		    bvl
		    (add1 lex-level)
		    senv
		    henv
		    cenv)
	  (when (or debug-mode (lambda-closed? x))
		(emit-load dest				; set up closure name
			   (if (null? (lambda-debug x))
			       0			; use R0 for '()
			       (list 'QUOTE (lambda-debug x))))
		(emit 'CLOSE dest
		             dest
			     (list label (lambda-nargs x)))
		(set! last-label label)
		(continue dest tr?)))))

  (gen-if
      (lambda (x dest tr?)
	(let ((pred (if-pred x))
	      (then (if-then x))
	      (else (if-else x)))
	  (gen pred dest #!false)
	  (restore-regs dest)
	  (let* ((tos0 tos)
		 (out  (gensym 'I)))
	    (cond			        ; (if a b '())
	        ((equal? else ''())
		 (emit-live dest)
		 (emit 'JUMP out 'NULL? dest)
		 (gen then dest tr?)
		 (restore-tos tos0 tr?)
		 (emit-label out)
		 (continue dest tr?)
		 )			        ; (if a '() c)
		((equal? then ''())
		 (emit 'NOT dest dest)
		 (emit-live dest)
		 (emit 'JUMP out 'NULL? dest)
		 (gen else dest tr?)
		 (restore-tos tos0 tr?)
		 (emit-label out)
		 (continue dest tr?)
		 )			       ; (if a a c)
		((or (eq? pred then)
		     (and (memq (car pred)	; no side effects?
				'(%%get-global%%
				  %%get-scoops%%
				  %%get-fluid%%))
			  (equal? pred then)))
		 (emit-live dest)
		 (emit 'JUMP out 'T? dest)
		 (gen else dest tr?)
		 (restore-tos tos0 tr?)
		 (emit-label out)
		 (continue dest tr?)
		 )			       ; (if a b c)
		(else
		 (let ((lelse (gensym 'L)))
		   (emit-live dest)
		   (emit 'JUMP lelse 'NULL? dest)
		   (gen then dest tr?)
		   (restore-tos tos0 tr?)
		   (when (not tr?)
			 (emit-live dest)
			 (emit-jump out))
		   (emit-label lelse)
		   (gen else dest tr?)
		   (restore-tos tos0 tr?)
		   (when (not tr?)
			 (emit-label out)))))
	    ))))

  (gen-ccc
      (lambda (x dest tr?)
	(let* ((fun (cadr x))
	       (info (assq fun cenv)))	 ; CENV = () in debug mode
	  (if info
	      (let* ((label       (cadr info))		; open call
		     (delta-level (- lex-level
				     (caddr info)))
		     (delta-heap  (- (length henv)
				     (length (cadddr info)))))
		(set! last-label label)
		(restore-regs dest)
		(if (and tr? ( >= delta-level 0))
		    (emit 'CALL
			  `(OPEN-TR ,label ,delta-level ,delta-heap)
			  'CC)
		    (begin
		       (save-regs dest)
		       (emit 'CALL
			     `(OPEN ,label ,delta-level ,delta-heap)
			     'CC)
		       (emit-copy dest 1)
		       (continue dest tr?))))
	      (begin					; closed call
		 (gen fun dest #!false)
		 (restore-regs dest)
		 (if tr?
		     (emit 'CALL 'CLOSED-TR 'CC dest)
		     (begin
		        (save-regs dest)
		        (emit 'CALL 'CLOSED 'CC dest)
			(emit-copy dest 1))))))))

  (gen-begin
      (lambda (x dest tr?)
	(if (null? (cdr x))
	    (gen (car x) dest tr?)
	    (begin
	       (gen (car x) dest #!false)
	       (gen-begin (cdr x) dest tr?)))))

  (gen-apply
      (lambda (x dest tr?)
	(let ((fun   (cadr x))
	      (arg   (caddr x))
	      (dest1 (add1 dest)))
	  (gen arg dest #!false)
	  (gen fun dest1 #!false)
	  (restore-regs dest)
	  (if tr?
	      (emit 'CALL 'CLOSED-APPLY-TR dest1 dest)
	      (begin
	         (save-regs dest)
		 (emit 'CALL 'CLOSED-APPLY dest1 dest)
		 (emit-copy dest 1))))))

  (gen-let
      (lambda (x dest tr?)
	(let ((fun  (car x))
	      (args (cdr x)))
	  (gen-args args dest)
	  (restore-regs dest)
	  (let ((save-henv henv)
		(save-senv senv)
		(save-cenv cenv))
	    (set! henv (cons '() henv))
	    (let ((newdest (extend-bvl (lambda-bvl fun) dest)))
	      (gen (lambda-body fun) newdest tr?)
	      (when (not tr?)
		    (restore-regs newdest)
		    (drop dest)
		    (drop-env (- (length henv)		; normally 1 or 0
				 (length save-henv)))
		    (emit-copy dest newdest))
	      (set! henv save-henv)
	      (set! senv save-senv)
	      (set! cenv save-cenv))))))


  ;;
  ;; LETREC pairs must be handled VERY carefully!  We pass over them three
  ;; times in order to get CENV, SENV, and (especially) HENV correct when
  ;; referenced from within the pair expressions.
  ;;
  ;; Pass 1 - Determine which runtime variables must be heap allocated
  ;;	      and reserve space for them on the heap-allocated stack.
  ;;          When done, HENV and SENV reflect the proper lexical
  ;;          environment for generating the code for the body AND the
  ;;          pairs themselves.
  ;;
  ;; Pass 2 - Add all compile-time only variables and "well-behaved"
  ;;          runtime variables to CENV.  Note that CENV entries include
  ;;          the HENV in effect at the time of CLOSURE, which is AFTER all
  ;;          pair IDs have been allocated homes (in the first pass).
  ;;
  ;; Pass 3 - Generate code to assign pair expression values to pair IDs.
  ;;          Note that Passes 1 and 3 must have exactly the same behavior
  ;;          with respect to maintaining DEST.  Thus, they have the same
  ;;          general structure.

  (gen-letrec
      (lambda (x dest tr?)
	(let ((save-henv henv)
	      (save-senv senv)
	      (save-cenv cenv))
	  (set! henv (cons '() henv))			; add a rib
	  (let ((newdest (gen-pairs (letrec-pairs x) dest))
		(body    (letrec-body x)))
	    (gen body newdest tr?)
	    (when (not tr?)
		  (restore-regs newdest)
		  (drop dest)
		  (drop-env (- (length henv)		; normally 1 or 0
			       (length save-henv)))
		  (emit-copy dest newdest))
	    (set! henv save-henv)
	    (set! senv save-senv)
	    (set! cenv save-cenv)))))

  (gen-pairs
      (lambda (pairs dest)
	(gen-pairs-1 pairs dest)
	(when (not debug-mode)
	      (gen-pairs-2 pairs))
	(gen-pairs-3 pairs dest)))

  (gen-pairs-1
      (lambda (pairs dest)
	(if (null? pairs)
	    (if (null? (car henv))
		(set! henv (cdr henv))
		(begin
		  (set-car! henv (reverse! (car henv)))
		  (emit 'PUSH-ENV (car henv))))
	    (let ((id  (caar pairs))
		  (exp (cadar pairs)))
	      (gen-pairs-1
	          (cdr pairs)
		  (if (or debug-mode (id-rtv? id))
		      (if (or debug-mode (id-heap? id))
			  (begin          ; heap-alloc lex var
			     (set-car! henv
				       (cons (id-name id) (car henv)))
			     dest)
			  (begin          ; stack/reg-alloc lex var
			     (set! senv
				   (cons (cons id
					       (cons (+ reg-base dest)
						     lex-level))
					 senv))
			     (add1 dest)))  ; reserve a register
		      dest))))))


  (gen-pairs-2
      (lambda (pairs)
	(when pairs		         ; not called in debug mode
	      (let ((id  (caar pairs))
		    (exp (cadar pairs)))
		(when (or (not (id-rtv? id))
			  (and (not (id-set!? id))
			       (eq? (car exp) 'lambda)
			       (not (negative? (lambda-nargs exp)))))
		      (set! cenv
			    (cons (list id (lambda-label exp)
					(add1 lex-level) henv)
				  cenv))))
	      (gen-pairs-2 (cdr pairs)))))

  (gen-pairs-3
      (lambda (pairs dest)
	(if (null? pairs)
	    dest
	    (let ((id  (caar pairs))
		  (exp (cadar pairs)))
	      (gen exp dest #!false)
	      (restore-regs dest)
	      (gen-pairs-3
	          (cdr pairs)
		  (if (or debug-mode (id-rtv? id))
		      (if (or debug-mode (id-heap? id))
			  (begin
			    (when (not (equal? exp '(quote ())))
				  (emit 'STORE (list 'HEAP (id-name id))
					       dest))
			    dest)
			  (add1 dest))
		      dest))))))

  ;; Bound variable lists are similar to LETREC pairs, but much easier to
  ;; deal with, since they are always runtime variables.  Thus, EXTEND-BVL
  ;; is a simplified combination of GEN-PAIRS-1 (setting up HENV and SENV)
  ;; and GEN-PAIRS-3 (emitting PUSH-ENV instructions when needed).

  (extend-bvl
      (lambda (bvl dest)
	(extend-bvl-1 bvl dest)
	(extend-bvl-2 bvl dest)))

  (extend-bvl-1
      (lambda (bvl dest)
	(if (null? bvl)
	    (if (and (not debug-mode)
		     (null? (car henv)))
		(set! henv (cdr henv))		; null env frame
		(begin
		  (set-car! henv (reverse! (car henv)))
		  (emit 'PUSH-ENV (car henv))))
	    (let ((id (car bvl)))
	      (if (or debug-mode (id-heap? id))
		  (set-car! henv (cons (id-name id) (car henv)))
		  (set! senv
			(cons (cons id
				    (cons (+ reg-base dest)
					  lex-level))
			      senv)))
	      (extend-bvl-1 (cdr bvl) (add1 dest))))))

  (extend-bvl-2
      (lambda (bvl dest)
	(if (null? bvl)
	    dest
	    (let ((id (car bvl)))
	      (when (or debug-mode (id-heap? id))
		    (emit 'STORE (list 'HEAP (id-name id)) dest))
	      (extend-bvl-2 (cdr bvl) (add1 dest))))))

  (gen-application
      (lambda (x dest tr?)
	(let ((fun (car x)))
	  (let ((nargs (length (cdr x))))
	    (when (not (zero? nargs))
		  (gen-args (cdr x) dest))
	    (let ((info (assq fun cenv)))    ; CENV = () in debug mode
	      (if info
		  ;; open call
		  (let* ((label       (cadr info))
			 (delta-level (- lex-level
					 (caddr info)))
			 (delta-heap  (- (length henv)
					 (length (cadddr info)))))
		    (when (not (= nargs (lambda-nargs (id-init fun))))
			  (syntax-error "Wrong number of arguments in call"
					(id-name fun)))
		    (set! last-label label)
		    (restore-regs dest)
		    (if (and tr?			; tail recursive
			     ( >= delta-level 0))	; frame not needed
			(begin
			   (move-regs dest 1 nargs)
			   (if (zero? delta-level)
			       (begin
				  (drop-all)
				  (drop-env delta-heap)
				  (emit-live nargs)
				  (emit-jump label))
			       (emit 'CALL
				     `(OPEN-TR ,label ,delta-level
					       ,delta-heap)
				     (list nargs))))
			(begin
			   (save-regs dest)
			   (move-regs dest 1 nargs)
			   (emit 'CALL
				 `(OPEN ,label ,delta-level ,delta-heap)
				 (list nargs))
			   (emit-copy dest 1)
			   (continue dest tr?))))
		  ;; closed call
		  (let ((funreg (+ dest nargs))	; compute function here
			(nargs1 (+ nargs 1)))	; then move it here
		    ;; must compute function before moving regs down
		    (gen fun funreg #!false)
		    (restore-regs dest)
		    (if tr?
			(begin
			   (move-regs dest 1 nargs1)
			   (emit 'CALL
				 'CLOSED-TR (list nargs) nargs1))
			(begin
			   (save-regs dest)
			   (move-regs dest 1 nargs1)
			   (emit 'CALL
				 `CLOSED (list nargs) nargs1)
			   (emit-copy dest 1))))))))))

  (out-of-registers!
      (lambda ()
	(error " *** Compiler ran out of registers ***")))

  (gen-args
      (lambda (args dest)
	(when args
	      (when (> dest max-regnum)
		    (out-of-registers!))
	      (gen (car args) dest #!false)
	      (gen-args (cdr args)(add1 dest)))))

  (gen-primitive
      (lambda (x dest tr?)
	(let ((primop (car x)))
    ;;	  (when (null? primop)
    ;;	        (set! **null-primop** x)
    ;;		(writeln "++ Null primop found, saved in **NULL-PRIMOP**"))
	  (cond (( >= (+ dest (length (cdr x))) max-regnum)
		 (out-of-registers!))
		((memq primop '(%%get-global%%  %%set-global%%
				%%get-scoops%%  %%set-scoops%%
				%%def-global%%  %%get-fluid%% 
				%%set-fluid%%   %%bind-fluid%%
				%%unbind-fluid%%))
		 (case primop
		   (%%get-global%%   (gen-global-ref x dest tr? 'HEAP))
		   (%%set-global%%   (gen-global-set x dest tr? 'HEAP))
		   (%%get-scoops%%   (gen-global-ref x dest tr? 'GLOBAL))
		   (%%set-scoops%%   (gen-global-set x dest tr? 'GLOBAL))
		   (%%def-global%%   (gen-global-def x dest tr?))
		   (%%get-fluid%%    (gen-fluid-ref x dest tr?))
		   (%%set-fluid%%    (gen-fluid-set x dest tr?))
		   (%%bind-fluid%%   (gen-fluid-bind x dest tr?))
		   (else             (gen-fluid-unbind x dest tr?))))
                ((memq primop '(%xesc))          ;variable-length instructions
                 (let* ((inst-length (cadr x))
                        (src-regs (gen-prim-args (cddr x) dest))
                        (newdest (if (null? src-regs)
                                     dest
                                     (car src-regs)))
                        (instr `(,primop ,newdest ,inst-length ,@src-regs)))
                   (restore-regs dest)
                   (emit* instr)
                   (emit-copy dest newdest)
                   (continue dest tr?)))
		((and (memq primop '(+ - * / ))
		      (eq? (car (caddr x)) 'quote)
		      (integer? (cadr (caddr x)))
		      (< (abs (cadr (caddr x))) 128))
		 (gen (cadr x) dest #!false)
		 (restore-regs dest)
		 (emit (cdr (assq primop
				  '((+ . %+imm)(- . %+imm)
				    (* . %*imm)(/ . %/imm))))
		       dest
		       dest
		       (if (eq? primop '-)
			   `(quote ,(minus (cadr (caddr x))))
			   (caddr x)))
		 (continue dest tr?))
		(else
		 (let* ((src-regs (gen-prim-args (cdr x) dest))
			(newdest  (if (null? src-regs)
				      dest
				      (car src-regs)))
			(instr    (cons primop (cons newdest src-regs))))
		   (restore-regs dest)
		   (emit* instr)
		   (emit-copy dest newdest)
		   (continue dest tr?)))))))

  (gen-prim-args
      (lambda (args dest)
	(cond ((null? args)                    ; 0 args
	       '())
	      ((null? (cdr args))              ; 1 arg
	       (gen (car args) dest #!false)
	       (list dest))
	      (else
	       (let ((arg1 (car args))
		     (arg2 (cadr args))
		     (dest1 (+ dest 1)))
		 (if (and (memq (car arg1) '(t quote %%get-global%%))
			  (not (memq (car arg2) '(t quote %%get-global%%))))
		     (begin
		       (gen arg2 dest #!false)
		       (gen arg1 dest1 #!false)	; lex var or constant
		       (cons dest1
			     (cons dest
				   (gen-prim-args (cddr args)(+ dest 2)))))
		     (begin
		        (gen arg1 dest #!false)
			(cons dest (gen-prim-args (cdr args) dest1)))))))))

  (gen-global-ref
      (lambda (x dest tr? kind)
	(emit-load dest (list kind (cadr (cadr x))))
	(continue dest tr?)))

  (gen-global-set
      (lambda (x dest tr? kind)
	(let ((symbol (cadr (cadr x)))
	      (value (caddr x)))
	  (gen value dest #!false)
	  (restore-regs dest)
	  (emit 'STORE (list kind symbol) dest)
	  (continue dest tr?))))

  (gen-global-def
      (lambda (x dest tr?)
	(let ((symbol (cadr (cadr x)))
	      (value (caddr x)))
	  (gen value dest #!false)
	  (restore-regs dest)
	  (emit 'STORE (list 'GLOBAL-DEF symbol) dest)
	  (emit-load dest (cadr x))
	  (continue dest tr?))))

  (gen-fluid-ref
      (lambda (x dest tr?)
	(emit-load dest (list 'FLUID (cadr (cadr x))))
	(continue dest tr?)))

  (gen-fluid-set
      (lambda (x dest tr?)
	(let ((symbol (cadr (cadr x)))
	      (value (caddr x)))
	  (gen value dest #!false)
	  (restore-regs dest)
	  (emit 'STORE (list 'FLUID symbol) dest)
	  (continue dest tr?))))

  (gen-fluid-bind
      (lambda (x dest tr?)
	(let ((symbol (cadr (cadr x)))
	      (value (caddr x)))
	  (gen value dest #!false)
	  (restore-regs dest)
	  (emit 'BIND-FLUID symbol dest)
	  (continue dest tr?))))

  (gen-fluid-unbind
      (lambda (x dest tr?)
	(let ((symlist (cadr (cadr x))))
	  (emit 'UNBIND-FLUIDS symlist)
	  (continue dest tr?))))

  (continue
      (lambda (dest tr?)
	(when tr?					; tail recursive
	      (restore-regs dest)
	      (if (not (= dest 1))
		  (emit-copy 1 dest))
	      (emit 'CALL 'EXIT 1))))

  (emit
      (lambda instr
	 (set! code (cons instr code))))

  (emit*
      (lambda (instr)
	(set! code (cons instr code))))

  (emit-label
      (lambda (tag)
	(set! code (cons tag code))))

  (emit-load
      (lambda args
	 (set! code (cons (cons 'LOAD args) code))))

  (emit-copy
      (lambda (dest src)
	(if (not (= dest src))
	    (emit 'LOAD dest src))))
  (emit-live
      (lambda (reg)
	(emit 'LIVE
	      (if (zero? reg)
		  '()
		  (cons 1 reg)))))

  (emit-jump
      (lambda (label)
	(set! code (cons (cons 'JUMP (cons label '(ALWAYS)))
			 code))))

  (emit-push
      (lambda (reg)
	(letrec
	  ((pushback
	     (lambda (reg prev curr)
	       (cond ((or (null? curr)        ; start
			  (atom? (car curr))  ; label
			  (memq (caar curr)
				'(POP PUSH DROP JUMP CALL))
			  (and (not (atom? (cdar curr)))
			       (equal? reg (cadar curr))
			       (or (not (eq? (caar curr) 'LOAD))
				   (not (number? (caddr (car curr)))))))
		      (let ((tail (cons `(PUSH () ,reg) curr)))
			(if (null? prev)
			    (set! code tail)
			    (set-cdr! prev tail))))
		     ((and (eq? (caar curr) 'LOAD)
			   (= reg (cadar curr))
			   (number? (caddr (car curr))))
		      (pushback (caddr (car curr)) curr (cdr curr)))
		     (t (pushback reg curr (cdr curr)))))))
	  (begin
	     (pushback reg '() code)
	     (set! tos (add1 tos))
	     (if (not (= tos (+ reg reg-base)))
		 (error " *** EMIT-PUSH error: " reg reg-base tos))))))

  (emit-pop
      (lambda (reg)
	(if (not (= tos (+ reg reg-base)))
	    (error " *** EMIT-POP error: " reg reg-base tos))
	(emit 'POP reg)
	(set! tos (sub1 tos))))

  (save-regs
      (lambda (reg)
	(let ((reg-to-push (add1 (- tos reg-base))))
	  (when ( < reg-to-push reg)
		(emit-push reg-to-push)
		(save-regs reg)))))

  (restore-regs
      (lambda (reg)
	(let ((reg-to-pop (- tos reg-base)))
	  (when ( >= reg-to-pop reg)
		(emit-pop reg-to-pop)
		(restore-regs reg)))))

  (restore-tos
      (lambda (tos0 tr?)
	(cond (tr?            (set! tos tos0))
	      (( > tos tos0)  (emit-pop (- tos reg-base))
			      (restore-tos tos0 tr?))
	      (( < tos tos0)  (emit-push (add1 (- tos reg-base)))
			      (restore-tos tos0 tr?)))))

  (drop-all
      (lambda ()
	(let ((count (add1 tos)))
	  (when ( > count 0)
		(emit 'DROP (list count))
		(set! tos -1)))))

  (drop		      ; drop down to and including REG
      (lambda (reg)
	(let* ((newtos (sub1 (+ reg reg-base)))
	       (count  (- tos newtos)))
	  (when ( > count 0)
		(emit 'DROP (list count))
		(set! tos newtos)))))

  (drop-env
      (lambda (count)
	(when (> count 0)
	      (emit 'DROP-ENV (list count)))))

  (move-regs
      (lambda (from to count)
	(if ( > from to)
	    (when ( > count 0)
		  (emit-copy to from)
		  (move-regs (add1 from)(add1 to)(sub1 count))))))

;--------------!
               )                              ;; body of gen-code
               (let ((save-henv henv)
		     (save-senv senv)
		     (save-cenv cenv))
		 (set! henv (cons '() henv))		; add a rib
		 (let ((newdest (if (eq? entry-name '==main==)
				    1
				    (extend-bvl bvl 1))))
		   (gen body newdest #!true)
		   (set! compiled-lambda-list
			 (cons (cons entry-name
				     (cons last-label (reverse! code)))
			       compiled-lambda-list))
		   (set! henv save-henv)
		   (set! senv save-senv)
		   (set! cenv save-cenv)
		   )))))

  (flatten
      (lambda (cl)
	(if (null? cl)
	    cl
	    (let* ((first      (car cl))
		   (label      (car first))
		   (last-label (cadr first))
		   (oplist     (cddr first))
		   (rest       (flat** last-label (cdr cl) '())))
	      (cons label
		    (append! oplist
			     (flatten rest)))))))
			        

  (flat**
      (lambda (label a b)
	(cond ((null? label)         a)
	      ((null? a)             b)
	      ((eq? label (caar a))  (append! a b))
	      (t  (flat** label (cdr a) (cons (car a) b))))))

;------!
        )
       (begin                                 ;; body of pcs-gencode
	  (gen-code '==main== exp '() 1 '() '() '())
	  (flatten compiled-lambda-list)
	  ))))
