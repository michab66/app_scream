
;      -*- Mode: Lisp -*-                             Filename:  ppeep.s

;                     Last Revision:  1-Oct-85 1630ct

;--------------------------------------------------------------------------;
;                                                                          ;
;                         TI SCHEME -- PCS Compiler                        ;
;                  Copyright 1985 (c) Texas Instruments                    ;
;                                                                          ;
;                              David Bartley                               ;
;                                                                          ;
;                        Post-Codegen Optimization                         ;
;                                                                          ;
;--------------------------------------------------------------------------;
;                                                                          ;
; Note:  The optimization TEST+JUMP-NULL? ==> JUMP-NOT-TEST has not been   ;
;        implemented because peep2 can't reliably tell when TEST is dead.  ;
;                                                                          ;
;									   ;
; Revisions :								   ;
;  6/1/87 rb - Modified p2-substitute, so as not to monkey with %xesc      ;
;  6/3/87 tc - Modified p1 register substitution to understand %xesc       ;
;									   ;
;									   ;
;--------------------------------------------------------------------------;


(define pcs-postgen
  (lambda (code)
    (letrec
     (
;----!

  (peep1
   (lambda (code)
     (cond (pcs-permit-peep-1 (p1 code '()))
	   (pcs-permit-peep-2 (reverse! code))
	   (t code))))

  (p1
   (lambda (next acc)
     (if (null? next)
	 (begin
	    (p1-forget-all)
	    acc)
	 (let ((rest  (cdr next))
	       (instr (car next)))
	   (cond ((or (atom? instr)			; label
		      (number? (car instr)))		; label
		  (when (and acc
			     (cdr acc)
			     (not (atom? (car acc)))
			     (eq? (caar acc) 'JUMP)
			     (equal? (cadar acc) instr))
			(set! acc (cdr acc)))   ; delete "JUMP $+1"
		  (p1-forget-all))
		 ((memq (car instr) '(JUMP CALL LIVE))
		  (p1-forget-all))
		 ((eq? (car instr) 'LOAD)
		  (p1-propagate (cddr instr))		; src reg
		  (p1-forget (cdr instr))		; dest reg
		  (p1-remember (cadr instr)		; dest <== src
			       (caddr instr))
		 )
		 ((eq? (car instr) '%XESC)		
             	  ; %xesc assumes the dest reg will be equal     tc - 6/3/87
		  ; to the third operand (cadddr instr)
		  (let ((dest (cadr instr)))
		    (p1-propagate-all (cdr instr))
	 	    (p1-forget (cdr instr))		; dest reg
		    (p1-forget dest)			; old dest reg
         	    (p1-remember (cadr instr)		; dest <== src
 	 		         (cadddr instr))
		    (p1-remember dest 			; old dest <== src
				 (cadddr instr))
		  )
		 )
		 ((not (atom? (cdr instr)))
		  (p1-propagate-all (cddr instr))	; src regs
		  (p1-forget (cdr instr)))		; dest reg
		 (t '()))
	   (set-cdr! next acc)
	   (p1 rest next)))))

  (p1-propagate
   (lambda (s*)   ; (src ...)
     (when (not (atom? s*))
	   (let ((s (car s*)))
	     (when (number? s)
		   (let ((sub (vector-ref reg-table s)))
		     (when sub				; any sub
			   (set-car! s* sub))))))))

  (p1-propagate-all
   (lambda (s*)   ; (src ...)
     (when (not (atom? s*))
	   (let ((s (car s*)))
	     (when (number? s)
		   (let ((sub (vector-ref reg-table s)))
		     (when (number? sub)		; regs only
			   (set-car! s* sub)))))
	   (p1-propagate-all (cdr s*)))))		; cdr down

  (p1-remember
   (lambda (dest src)
     (when (or (number? src)				; reg?
	       (and (not (atom? src))			; constant
		    (eq? (car src) 'quote)))
	   (vector-set! reg-table dest src)
	   (set! reg-table-max
		 (max reg-table-max
		      (if (and (number? src)(> src dest))
			  src
			  dest))))))

  (p1-forget
   (lambda (d*)   ; (dest ...)
     (when (not (atom? d*))
	   (let ((d (car d*)))
	     (when (number? d)   ; reg
		   (vector-set! reg-table d #!false)
		   (p1-forget-uses d))))))

  (p1-forget-uses
   (lambda (reg)
     (letrec ((loop (lambda (v i reg)
		      (when (not (negative? i))
			    (if (equal? (vector-ref v i) reg)
				(vector-set! v i #!false))
			    (loop v (sub1 i) reg)))))
	  (loop reg-table reg-table-max reg))))

  (p1-forget-all
   (lambda ()
     (vector-fill! reg-table #!false)))


;;; p2 -- peephole optimizer pass 2

;;; Purposes:
;;;
;;;    1. Destructively reverse the code list (previously reversed by the
;;;       first pass), returning it to forward order.
;;;
;;;    2. Eliminate dead code
;;;
;;;       Delete instructions whenever the destination register is dead and
;;;       there are no side effects.
;;;
;;;       Maintain live/dead info: destination registers are dead prior to
;;;       assignment, source registers become live.  LIVE directives and
;;;       arguments to CALLs also control liveness.
;;;
;;;       Assumption: every JUMP is immediately preceded by a LIVE.
;;;
;;;    3. Target registers
;;;
;;;       Delay register moves (only), such as (LOAD A B).  Mark register A
;;;       as dead, register B as live.
;;;
;;;       Force delayed loads whenever register A is used or a label, CALL,
;;;       or JUMP occurs.
;;;
;;;       Substitute register A for register B and remove the (LOAD A B)
;;;       from the delayed list whenever register B is the destination of
;;;       an instruction.
;;;
;;;    4. Other optimizations
;;;
;;;       Eliminate no-ops:  (LOAD A A)
;;;
;;;       Commute operands:  (+ A B A) ==> (+ A A B)
;;;
;;;
;;; Data Structures:
;;;
;;;    REG-TABLE [0..63]
;;;
;;;       Entry I is #!FALSE iff register I is "live"
;;;
;;;    DELAY-LIST
;;;
;;;       "Delayed" register moves are maintained in the form:
;;;
;;;                        ((LOAD Ai Bi) ...)
;;;
;;;       where each Ai and Bi is a register number, no Ai=Aj, no Ai=Bj,
;;;       and no Bi=Bj.  The P2-DELAY routine decides whether to delay a
;;;       given (LOAD A B), based on the following considerations:
;;;
;;;       (= A B)  : Can't happen, because P2 previously deletes these
;;;       no-ops [p2-dead].
;;;
;;;       (= A Ai) : Can't happen, because Ai is "dead" and P2 would have
;;;       deleted this operation [p2-dead].
;;;
;;;       (= A Bi) : Can't happen, because P2 would previously have
;;;       substituted the corresponding Ai for A [p2-substitute], making
;;;       this (LOAD Ai B), and no Ai=Bj.  (???)
;;;
;;;       (= B Ai) : Can't happen, because P2 would have forced out any
;;;       delayed (LOAD Ai Bi) [p2-sources].
;;;
;;;       (= B Bi) : CAN happen.  We modify the current instruction so we
;;;       can continue to delay the previous (LOAD Ai Bi), as follows.
;;;
;;;          Example:   (load 3 5) ... (load 4 5)
;;;
;;;             When we see the (LOAD 3 5), we have already delayed the
;;;             (LOAD 4 5).  Thus, we change (LOAD 3 5) into (LOAD 3 4),
;;;             make register 4 "live", and continue to delay (LOAD 4 5).
;;;
;;;	  B is live : CAN happen.  Don't delay the load, since the values
;;;	  of both A and B are needed.
;;;
;;;       otherwise : delay the (LOAD A B).
;;;

  (peep2
   (lambda (code)
     (cond (pcs-permit-peep-2 (p2 code '()))
	   (pcs-permit-peep-1 (reverse! code))
	   (t code))))

  (p2
   (lambda (next acc)
     (if (null? next)
	 acc
	 (let ((rest  (cdr next))
	       (instr (car next)))
	   (begin
	      (set-cdr! next acc)   ; assume we will keep it
	      ;; don't use ACC past here
	      (if (or (atom? instr)
		      (number? (car instr)))
		  (p2 rest (p2-force-all next))		; label
		  (let ((op (car instr)))
		    (cond
		       ((eq? op 'JUMP)			; JUMP
			(p2-jump instr rest next))

		       ((eq? op 'CALL)			; CALL
			(p2-call instr rest next))

		       ((eq? op 'LIVE)			; LIVE
			(p2-live instr rest next))

		       ((p2-dead? instr)		; result not needed
			(p2 rest (cdr next)))		; delete it

		       (t
			(p2-substitute instr)
			(if (eq? op 'LOAD)
			    (p2-load instr rest next)
			    (begin
			       (let ((dest (cadr instr)))
				 (when (number? dest)
				    (p2-force dest next delay-list '())
				    (p2-kill dest)))
			       (p2-sources     ; make the src regs live
				  (cddr instr) next)
			       (p2-keep rest instr next))))))))))))


;;; p2-jump -- Process JUMP instructions.

  (p2-jump
   (lambda (instr rest next)
     (p2 rest
	 (p2-sources (cdddr instr)
		     (p2-force-all next)))))


;;; p2-call -- Process CALL instructions.

  (p2-call
   (lambda (instr rest next)
     (vector-fill! reg-table #!true)		    ; make all regs dead
     (let ((next (p2-sources (cddr instr) 
			     (p2-force-all next)))) ; make src regs live
       (if (not (atom? (caddr instr)))
	   (p2-make-live 1 (car (caddr instr))))    ; number of args
       (p2 rest next))))

;;; p2-live -- Process LIVE directives.

  (p2-live
   (lambda (instr rest next)
     (vector-fill! reg-table #!true)		; make all regs dead
     (let ((range (cadr instr)))		; then make some live
       (when (not (null? range))
	     (p2-make-live (car range)(cdr range))))
     (p2 rest next)))

  (p2-make-live
   (lambda (lo hi)
     (when ( >= hi lo)
	   (vector-set! reg-table hi #!false)   ; make reg live
	   (p2-make-live lo (sub1 hi)))))

;;; p2-load -- Process LOAD instructions.

  (p2-load
   (lambda (instr rest next)
     (let ((dest (cadr instr))
	   (src  (caddr instr)))
       (if (equal? dest src)         ; no-op?
	   (p2 rest (cdr next))      ; delete it
	   (let ((live-src? (and (number? src)
				 (null? (vector-ref reg-table src)))))
	     (p2-force dest next delay-list '())
	     (p2-kill dest)
	     (p2-sources (cddr instr) next)
	     (let ((acc (cdr next)))
	       (if (and (not live-src?)
			(p2-delay next))  ; does (set-cdr! next ...)
		   (p2 rest acc)
		   (p2-keep rest instr next))))))))

;;; p2-substitute -- Attempt to substitute a delayed register for the
;;; destination of INSTR.  If the destination of INSTR is B and a 
;;; (LOAD A B) instruction has been delayed, then the destination is
;;; changed to A and the (LOAD A B) is forgotten.
;;;
;;; This substitution cannot be performed on %XESC instructions because
;;; %XESC assumes the destination is the same as the third operand

  (p2-substitute
   (lambda (instr)
     (letrec ((loop
	         (lambda (reg old new)
		   (if (null? old)
		       new
		       (let ((next (cdr old))
			     (src  (caddr (car old))))
			 (if (and (= reg src)
    				  ; don't substitute for %xesc   rb - 6/1/87
                                  (not (eq? (car instr) '%xesc)))
			     (begin			; replace the dest opd
				(p2-kill (cadr instr))  ; kill old dest reg
			        (set-car! (cdr instr)   ; subst new dest reg
					  (cadr (car old)))
				(append! next new))     ; forget it
			     (begin
			        (set-cdr! old new)
				(loop reg next old))))))))
	  (if delay-list
	      (let ((dest (cadr instr)))
		(if (number? dest)
		    (set! delay-list
			  (loop dest delay-list '()))))))))


;;; p2-kill -- Mark the register DEST as "dead".

  (p2-kill
   (lambda (dest)
     (if (number? dest)
         (vector-set! reg-table dest #!true))))


;;; p2-sources -- Process the source registers (SS) of an instruction:
;;;   1. Mark each source register as "live".
;;;   2. For each source operand OPD which is a register for which there is
;;;      a delayed assignment, force out the load, since this is the last
;;;      use of a previous value.
;;;   3. Return the updated code list, NEXT.

  (p2-sources
   (lambda (ss next)
     (if (null? ss)
	 next
	 (let ((opd (car ss)))
	   (if (number? opd)				; register
	       (begin
		  (vector-set! reg-table opd #!false)	; make it live
		  (p2-sources (cdr ss) 
			      (p2-force opd next delay-list '())))
	       (p2-sources (cdr ss) next))))))


;;; p2-force -- REG is a register which is being used as a source operand
;;; of the instruction which is at the head of CODE-LIST.  Thus, we must
;;; force out any delayed load which defines or uses REG, since the source
;;; operand must refer to the old value before reassignment (defines) and
;;; we can't eliminate registers with multiple uses.  Returns the updated
;;; CODE-LIST.

  (p2-force
   (lambda (reg code-list old new)
     (if (null? old)
	 (begin
	    (set! delay-list new)
	    code-list)
	 (let ((this (cdr old))
	       (dest (cadr (car old)))
	       (src  (caddr (car old))))
	   (if (or (= reg dest)
		   (= reg src))
	       (begin
		  (set-cdr! old (cdr code-list))
		  (set-cdr! code-list old)
		  (set! delay-list (append! this new))
		  code-list)
	       (begin
		  (set-cdr! old new)
		  (p2-force reg code-list this old)))))))


;;; p2-force-all -- Force all delayed register assignments out.  This is
;;; necessary at all jumps, calls, labels, etc.

  (p2-force-all
   (lambda (code-list)
     (when delay-list
	   (set-cdr! code-list
		     (append! delay-list (cdr code-list)))
	   (set! delay-list '()))
     code-list))


;;; p2-delay -- Delay instructions of the form (LOAD reg-A reg-B)

  (p2-delay
   (lambda (next)
     (let ((instr (car next)))
       (let ((dest (cadr instr))
	     (src  (caddr instr)))
	 (if (number? src)
	     (let ((delayed-load (p2-lookup src delay-list)))
	       (if delayed-load
		   (let ((delayed-dest (cadr delayed-load)))
		     (set-car! (cddr instr)
			       delayed-dest)     ; fix this one
		     (p2-make-live delayed-dest
				   delayed-dest) ; keep the other delayed
		     '())
		   (begin        ; delay this one
		      (set-cdr! next delay-list)
		      (set! delay-list next)
		      't)))
	     '())))))		     ; not a reg-reg move

  (p2-lookup
   (lambda (src dl)
     (cond ((null? dl)                 '())
           ((= src (caddr (car dl)))   (car dl))
           (t                          (p2-lookup src (cdr dl))))))


;;; p2-dead? -- Determine whether instruction INSTR may be considered
;;; redundant and thus deleted.  If the destination operand is "dead" and
;;; the instruction has no side effects, then the instruction is "dead".

  (p2-dead?
   (lambda (instr)
     (and (eq? (car instr) 'LOAD)			; no side effects
	  (number? (cadr instr))			; dest reg
	  (or (equal? (cadr instr)(caddr instr))
	      (not (null? (vector-ref reg-table (cadr instr))))))))


;;; p2-keep -- Keep the current instruction, INSTR (which is also the first
;;; item in NEXT).  If INSTR is a primitive that requires the first source
;;; operand to be the same as the destination register, add an appropriate
;;; LOAD in front and modify the instruction.

  (p2-keep
   (lambda (rest instr next)
     (let ((dest (cadr instr))
	   (src  (and (cddr instr)(caddr instr))))
       (cond ((or (not (number? dest))
	          (not (number? src))
	          (= dest src)
	          (memq (car instr) funny-primitives))
	      (p2 rest next))
	     ((member dest (cdddr instr)) 
	      (if (and (memq (car instr) commutative-primops)
		       (equal? dest (cadddr instr)))
		  (begin           		 ; swap source operands
		     (set-car! (cddr instr) dest)
		     (set-car! (cdddr instr) src)
		     (p2 rest next))
		  (begin
		     (set-cdr! next (cons (list 'LOAD dest 63)
					  (cdr next)))
		     (set-car! (cdr instr) 63)
		     (set-car! (cddr instr) 63)
		     (p2 rest (cons (list 'LOAD 63 src) next)))))
	     (t
	      (set-car! (cddr instr) dest)
	      (p2 rest (cons (list 'LOAD dest src) next)))))))


;;; data

  (funny-primitives '(LOAD cons car cdr caar cadr cdar cddr caaar caadr
		      cadar caddr cdaar cdadr cddar cdddr cadddr))

  (commutative-primops '(+ * = eq? eqv? equal? max min))

  (delay-list     '())
  (reg-table-max  0)
  (reg-table      (make-vector 64 #!false))

;----!
      )  
     (begin
        (when pcs-verbose-flag
	      (writeln "Codegen results:")
	      (pcs-princode code)
	      (newline))
	(let ((code1 (peep1 code)))
	  (when pcs-verbose-flag
		(writeln "Pass 1 optimization results:")
		(set! code1 (reverse! code1))
		(pcs-princode code1)
		(set! code1 (reverse! code1))
		(newline))
	  (let ((code2 (peep2 code1)))
	    (when pcs-verbose-flag
		  (writeln "Pass 2 optimization results:")
		  (pcs-princode code2)
		  (newline))
	    code2))))))


(define pcs-princode					; PCS-PRINCODE
  (lambda (code)
    (letrec
     (
;----!

  (tab  "	")
  (tab2 "		")
  (nlabels 0)
  (ninstrs 0)
  (nfields 0)

  (pcl
      (lambda (cl)
	(newline)
	(when cl
	      (let ((x (car cl)))
		(if (or (atom? x)			; label?
			(number? (car x)))
		    (begin
		      (set! nlabels (add1 nlabels))
		      (princ tab)
		      (princ x))     ; label
		    (begin
		      (set! ninstrs (add1 ninstrs))
		      (princ tab2)
		      (pc x tab)))       ; instruction
		(pcl (cdr cl))))))

  (pc
      (lambda (x spacer)
	(set! nfields (add1 nfields))
	(princ (car x))
	(when (cdr x)
	      (princ spacer)
	      (pc (cdr x) ", "))))

;----!
      )
     (pcl code)
     (writeln "    There are " nlabels " labels, "
	                       ninstrs " instructions, and "
			       nfields " fields.")
     )))
