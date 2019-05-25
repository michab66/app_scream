
;      -*- Mode: Lisp -*-				Filename:  pasm.s

;		      Last Revision:  3-Sep-85 1600ct

;--------------------------------------------------------------------------;
;									   ;
;			  TI SCHEME -- PCS Compiler			   ;
;		 Copyright 1985, 1987 (c) Texas Instruments		   ;
;									   ;
;			       David Bartley				   ;
;									   ;
;			     The PCS Assembler				   ;
;									   ;
;  rb 3/16/87 - added assembling variable-length instructions		   ;
;		(the XLI %xesc instruction is such)			   ;
;									   ;
;--------------------------------------------------------------------------;
;
;   Input:
;
;	AL is a list of assembly language instructions and labels.
;
;   Output:
;
;	The output is a list of the following components:
;
;		(PCS-CODE-BLOCK num-constants
;				len-code
;				(constant ...)
;				(code-byte ...))
;
;	NUM-CONSTANTS is the number of constants.
;
;	The list of constants contains all constants and names of globals
;	and fluids which are referenced by the code.  They are indexed from
;	0 to 255 from left to right.
;
;	The code is represented as a series of integers in the range
;	-255 .. 255 of length LEN-CODE.
;
;
;   Pass 1:
;
;	determine the "worst case" size of each instruction
;
;	assign tentative locations to labels based on "worst case" sizes
;
;   Pass 2:
;
;	identify instructions which can use short-form addressing
;
;	assign "final" locations to labels
;
;   Pass 3:
;
;	extract constants from the instructions and collect them
;
;	translate the instruction stream into an encoded byte stream
;
;--------------------------------------------------------------------------

(define pcs-assembler
  (lambda (al)
    (letrec
     (
;-----!

  (max-constants  255)	 ; constants are indexed 0..255
  (max-immediate  127)	 ; largest signed immediate value
  (min-immediate -128)	 ; smallest signed immediate value
  (max-delta-pc   127)	 ; maximum jump displacement (short form)

  (labels	  '())   ; ((label . locn) ...)
  (constants	  '())   ; (constant ...)
  (code 	  '())   ; (codebyte ...)
  (pc		    0)	 ; current simulated program counter

  (p1
   (lambda (al)
     (when al
	   (let ((x (car al)))
	     (if (or (atom? x)				; label?
		     (number? (car x)))
		 (set! labels (cons (cons x pc) labels))
		 (set! pc (+ pc (span x pc))))
	     (p1 (cdr al))))))

  (p2
   (lambda (al)
     (when al
	   (let ((x (car al)))
	     (if (or (atom? x)				; label?
		     (number? (car x)))
		 (let ((entry (assq x labels)))
		   (set-cdr! entry pc))
		 (set! pc (+ pc (span x pc))))
	     (p2 (cdr al))))))

  (p3
   (lambda (al)
     (when al
	   (let ((x (car al)))
	     (if (or (atom? x)				; label?
		     (number? (car x)))
		 (let ((entry (assq x labels)))
		   (when (not (=? pc (cdr entry)))
			 (writeln " *** ERROR in PCS-ASSEMBLER: " x)
			 (set! pc (cdr entry))))
		 (asm x))
	     (p3 (cdr al))))))

  (span
   (lambda (x old-pc)
     (let ((op (car x)))
       (case op
	 (LOAD	(if (and (not (atom? (caddr x)))
			 (eq? (car (caddr x)) 'STACK)
			 (not (zero? (caddr (caddr x)))))
		    4 3))
	 (STORE (if (and (not (atom? (cadr x)))
			 (eq? (car (cadr x)) 'STACK)
			 (not (zero? (caddr (cadr x)))))
		    4 3))
	 (JUMP	(let ((long  (length x))
		      (entry (assoc (cadr x) labels)))
		  (if (null? entry)
		      long
		      (let* ((new-pc (+ old-pc long))
			     (delta  (- (cdr entry) new-pc)))
			(if (<=? (abs delta) max-delta-pc)
			    (begin
			       (set-car! x 'HOP)  ; short jump
			       (sub1 long))
			    long)))))
	 (HOP	(length (cdr x)))
	 (CALL	(let ((kind (cadr x)))
		  (cond ((not (atom? kind))  5)
			((eq? kind 'EXIT)    1)
			((eq? (caddr x) 'CC) 2)
			(else		     3))))
	 (cons	4)
	 (CLOSE 5)
	 (LIVE	0)
	 (%XESC (let ((length (cadr (caddr x))))
		  (add1 length)))
	 (else
	  (cond ((memq op '(PUSH POP DROP DROP-ENV PUSH-ENV UNBIND-FLUIDS))
		 2)
		((memq op '(car cdr caar cadr cdar cddr caaar caadr
			    cadar caddr cdaar cdadr cddar cdddr cadddr
			    %%car %%cdr BIND-FLUID))
		 3)
		(else
		 (if (null? (cddr x))	      ; no source operands
		     (if (getprop op 'pcs*nilargop)
			 1		      ; no source or dest
			 2)		       ; dest only
		     (length (cdr x)))))
	  )))))

  (asm
   (lambda (x)
     (let ((op (car x)))
       (case op
	 (LOAD	(asm-load (reg (cadr x)) (caddr x)))
	 (STORE (asm-store (cadr x) (reg (caddr x))))
	 (JUMP	(asm-jump x))
	 (HOP	(asm-hop  x))
	 (CALL	(asm-call x))
	 (cons	(emit4 op (reg (cadr x))  (reg (caddr x)) (reg (cadddr x))))
	 (POP	(emit2 op (reg (cadr x))))
	 (PUSH	(emit2 op (reg (caddr x))))
	 (DROP	(emit2 op (car (cadr x))))
	 (DROP-ENV
		(emit2 op (car (cadr x))))
	 (PUSH-ENV
		(emit2 op (const (cadr x))))
	 (UNBIND-FLUIDS
		(emit2 op (length (cadr x))))
	 (BIND-FLUID
		(emit3 op (const (cadr x)) (reg (caddr x))))
	 (%XESC 		   ;format: (%xesc dest (quote len) r1 r2 ...)
                                   ;discard redundant 'dest' in (cadr x)
		(emitv-regs op (cadr (caddr x)) (cdddr x)))
	 (CLOSE (let* ((label	(car (cadddr x)))
		       (target	(cdr (assoc label labels)))
		       (delta	(- target (+ pc 5)))
		       (dest	(reg (cadr x)))
		       (nargs	(cadr (cadddr x))))
		  (emit5 op dest (lo-byte delta) (hi-byte delta) nargs)))
	 (LIVE	'())
	 (else
	  (cond ((memq op '(%%car %%cdr car cdr caar cadr cdar
			    cddr caaar caadr cadar caddr cdaar
			    cdadr cddar cdddr cadddr))
		 (emit3 op (reg (cadr x)) (reg (caddr x))))
		((memq op '(%+imm %*imm %/imm))
		 (emit3 op (reg (caddr x)) (cadr (cadddr x))))
		(t (emit1 op)
		   (if (null? (cddr x)) 	; no source operands
		       (if (getprop op 'pcs*nilargop)
			   '()                  ; no source or dest
			   (emit-regs (cdr x))) ; dest only
		       (emit-regs (cddr x)))))	; discard redundant 'dest'
	  )))))

  (asm-load
   (lambda (reg-dest src)
     (if (number? src)
	 (emit3 'LOAD reg-dest (reg src))
	 (case (car src)
	   (quote  (let ((exp (cadr src)))
		     (if (and (integer? exp)
			      (<=? exp max-immediate)
			      (>=? exp min-immediate))
			 (emit3 'LOAD-IMMEDIATE
				reg-dest
				exp)
			 (emit3 'LOAD-CONSTANT
				reg-dest
				(const exp)))))
	   (STACK  (let ((offset (cadr src))
			 (delta-level (caddr src)))
		     (if (zero? delta-level)
			 (emit3 'LOAD-LOCAL
				reg-dest
				offset)
			 (emit4 'LOAD-LEX
				reg-dest
				offset
				delta-level))))
	   (HEAP   (emit3 'LOAD-ENV
			  reg-dest
			  (const (cadr src))))
	   (GLOBAL (emit3 'LOAD-GLOBAL
			  reg-dest
			  (const (cadr src))))
	   (FLUID  (emit3 'LOAD-FLUID
			  reg-dest
			  (const (cadr src))))))))

  (asm-store
   (lambda (dest reg-src)
     (case (car dest)
       (STACK  (let ((offset (cadr dest))
		     (delta-level (caddr dest)))
		 (if (zero? delta-level)
		     (emit3 'STORE-LOCAL
			    reg-src
			    offset)
		     (emit4 'STORE-LEX
			    reg-src
			    offset
			    delta-level))))
       (HEAP   (emit3 'STORE-ENV
		      reg-src
		      (const (cadr dest))))
       (GLOBAL (emit3 'STORE-GLOBAL
		      reg-src
		      (const (cadr dest))))
       (GLOBAL-DEF
	       (emit3 'STORE-GLOBAL-DEF
		      reg-src
		      (const (cadr dest))))
       (FLUID  (emit3 'STORE-FLUID
		      reg-src
		      (const (cadr dest)))))))

  (asm-jump
   (lambda (x)
     (let* ((target (cdr (assoc (cadr x) labels)))
	    (len    (length x))
	    (delta  (- target (+ pc len)))
	    (regs   (cdddr x)))
       (emit1
	  (cdr (assq (caddr x)
		     '((ALWAYS . J_L)  (NULL?  . JN_L) (T?  . JNN_L)
		       (ATOM?  . JA_L) (NATOM? . JNA_L)(EQ? . JE_L)
		       (NEQ?   . JNE_L)))))
       (emit-regs regs)
       (emit-byte (lo-byte delta))
       (emit-byte (hi-byte delta))
       )))

  (asm-hop
   (lambda (x)
     (let* ((target (cdr (assoc (cadr x) labels)))
	    (len    (length (cdr x)))
	    (delta  (- target (+ pc len)))
	    (regs   (cdddr x)))
       (emit1
	  (cdr (assq (caddr x)
		     '((ALWAYS . J_S)  (NULL?  . JN_S) (T?  . JNN_S)
		       (ATOM?  . JA_S) (NATOM? . JNA_S)(EQ? . JE_S)
		       (NEQ?   . JNE_S)))))
       (emit-regs regs)
       (emit-byte delta)
       )))

  (asm-call
   (lambda (x)
     (let ((kind (cadr x)))
       (cond ((not (atom? kind))
	      (let* ((target	  (cdr (assoc (cadr kind) labels)))
		     (delta-level (caddr kind))
		     (delta-heap  (cadddr kind))
		     (delta	  (- target (+ pc 5))))
		(emit5 (cdr (assq (car kind)
				  (if (and (cddr x)(eq? (caddr x) 'CC))
				      '((OPEN . CCC) (OPEN-TR . CCC-TR))
				      '((OPEN . CALL)(OPEN-TR . CALL-TR)))))
		       (lo-byte delta) (hi-byte delta)
		       delta-level     delta-heap))
	      )
	     (else
	      (case kind
		(EXIT	    (emit1 kind))
		(CLOSED     (let ((fun-reg (reg (cadddr x))))
			      (if (eq? (caddr x) 'CC)
				  (emit2 'CCC-CLOSED fun-reg)
				  (emit3 'CALL-CLOSURE
					 fun-reg
					 (car (caddr x)))))) ; nargs
		(CLOSED-TR  (let ((fun-reg (reg (cadddr x))))
			      (if (eq? (caddr x) 'CC)
				  (emit2 'CCC-CLOSED-TR fun-reg)
				  (emit3 'CALL-CLOSURE-TR
					 fun-reg
					 (car (caddr x)))))) ; nargs
		(CLOSED-APPLY
			    (emit3 'APPLY-CLOSURE
				   (reg (caddr x))    ; funreg
				   (reg (cadddr x)))) ; argreg
		(CLOSED-APPLY-TR
			    (emit3 'APPLY-CLOSURE-TR
				   (reg (caddr x))    ; funreg
				   (reg (cadddr x)))) ; argreg
		))))))

  (const
   (lambda (exp)
     (let ((entry (memv exp constants)))
       (length (cdr (or entry
			(begin
			  (set! constants (cons exp constants))
			  (if (>? (length constants) max-constants)
			      (error "Constants table overflow in compiler")
			      constants))))))))

  (reg
   (lambda (index)
     (* 4 index)))

  (hi-byte
   (lambda (n)
     (let ((hi (quotient (abs n) 256)))
       (if (negative? n)
	   (if (zero? (remainder (abs n) 256))
	       (- 256 hi)
	       (- 255 hi))
	   hi))))

  (lo-byte
   (lambda (n)
     (let ((lo (remainder (abs n) 256)))
       (if (negative? n)
	   (if (zero? lo)
	       lo
	       (- 256 lo))
	   lo))))

  (emit-byte
   (lambda (byte)
     (set! code (cons byte code))
     (set! pc (add1 pc))))

  (emit-regs
   (lambda (x)
     (when x
	   (set! code (cons (reg (car x)) code))
	   (set! pc (add1 pc))
	   (emit-regs (cdr x)))))

  (emit-count
   (lambda (len)
     (set! code (cons len code))
     (set! pc (add1 pc))))

  (emit1
   (lambda (op)
     (let ((opcode (if pcs-binary-output
		       (abs (or (getprop op 'pcs*opcode)
				(error "++ undefined opcode" op)))
		       op)))
       (set! code (cons opcode code))
       (set! pc (+ pc 1)))))

  (emit2
   (lambda (op a)
     (emit1 op)
     (set! code (cons a code))
     (set! pc (+ pc 1))))

  (emit3
   (lambda (op a b)
     (emit1 op)
     (set! code (cons b (cons a code)))
     (set! pc (+ pc 2))))

  (emit4
   (lambda (op a b c)
     (emit1 op)
     (set! code (cons c (cons b (cons a code))))
     (set! pc (+ pc 3))))

  (emit5
   (lambda (op a b c d)
     (emit1 op)
     (set! code (cons d (cons c (cons b (cons a code)))))
     (set! pc (+ pc 4))))

  (emitv-regs
    (lambda (op len l)
      (emit1 op)
      (emit-count len)
      (emit-regs l)))

;-----!
      )
     (begin			;; body of pcs-assembler
	(p1 al)
	(when labels
	      (set! pc 0)
	      (p2 al))
	(set! pc 0)
	(p3 al)
	(set! constants (reverse! constants))
	(list 'PCS-CODE-BLOCK  (length constants)   pc
	      constants        (reverse! code))))))
