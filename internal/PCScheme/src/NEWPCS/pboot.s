
;      -*- Mode: Lisp -*-                             Filename:  pboot.s

;                     Last Revision:  3-Sep-85 1500ct

;--------------------------------------------------------------------------;
;                                                                          ;
;                         TI SCHEME -- PCS Compiler                        ;
;                  Copyright 1985 (c) Texas Instruments                    ;
;                                                                          ;
;                              David Bartley                               ;
;                                                                          ;
;                            Bootstrap Driver                              ;
;									   ;
;									   ;
;    This routine contains compiler-specific code which should be used	   ;
;    when compiling the compiler itself. It is generally loaded by the	   ;
;    file "COMPILE.ALL" which handles compilation of the compiler and	   ;
;    runtime routines.                                                     ;
;									   ;
;   The file contains compiler-type definitions and macro definitions	   ;
;   which must be included when compiling the compiler files.		   ;
;									   ;
;--------------------------------------------------------------------------;

(begin
  ;
  ;  Define aliases for the major parts of the compiler
  ;
  (alias pme   pcs-macro-expand)
  (alias psimp pcs-simplify)
  (alias pca   pcs-closure-analysis)
  (alias pmr   pcs-make-readable)
  (alias pcg   pcs-gencode)
  (alias ppeep pcs-postgen)
  (alias pal   pcs-princode)
  (alias pasm  pcs-assembler)

  ;
  ; Initialize compile-time variable definitions
  ;
  (set! pcs-local-var-count    0)
  (set! pcs-verbose-flag     #!true)
  (set! pcs-permit-peep-1    #!true)
  (set! pcs-permit-peep-2    #!true)
  (set! pcs-error-flag       #!false)
  (set! pcs-binary-output    #!false)

  ;
  ; Set up variables to hold compiler-intermediate data and timing info
  ;
  (define pme= '())
  (define psimp= '())
  (define pca= '())
  (define pcg= '())
  (define ppeep= '())
  (define pasm= '())
  (define problem)
  (define t-0)
  (define t-pme)
  (define t-psimp)
  (define t-pca)
  (define t-pcg)
  (define t-ppeep)
  (define t-pasm)
  )

;;; --------------------------------------------------------------------
;;;
;;;                         "Type definitions"
;;;
;;;	The following macros are used by the compiler itself and must
;;;     be defined when compiling the compiler. By keeping them here,
;;;     the macro definitions will not be around in the object files
;;;	of the compiler
;;;
;;; --------------------------------------------------------------------

(macro pcs-make-id					; PCS-MAKE-ID
  (lambda (form)
    (let ((name (cadr form)))
      `(begin
	 (set! pcs-local-var-count (+ pcs-local-var-count 1))
	 (list 'T           ; the symbol T, not #!TRUE !!
	       (cons ,name
		     pcs-local-var-count)
	       '() '() '())))))


;;;  ---- (t  (original-name . unique-number)  
;;;           funargsees? freeref? set!? . init) ----

(begin
  (syntax (id-name id)        (caadr id))
  (syntax (id-number id)      (cdadr id))
  (syntax (id-funargsees? id) (car  (cddr id)))
  (syntax (id-freeref? id)    (car  (cdddr id)))
  (syntax (id-set!? id)       (cadr (cdddr id)))
  (syntax (id-init id)        (cddr (cdddr id)))

  (syntax (id-rtv? id)
	  (or (id-set!? id)
	      (null? (id-init id))
	      (lambda-closed? (id-init id))))

  (syntax (id-heap? id)
	  (and (id-funargsees? id)
	       (id-freeref? id)
	       (id-rtv? id)))

  (syntax (set-id-funargsees? id val) (set-car! (cddr id) val))
  (syntax (set-id-freeref? id val)    (set-car! (cdddr id) val))
  (syntax (set-id-set!? id val)       (set-car! (cdr (cdddr id)) val))
  (syntax (set-id-init id val)        (set-cdr! (cdr (cdddr id)) val))
  )

;;;    ------ (lambda bvl body . (nargs label . closed)) ------

(begin
  (syntax (lambda-bvl x)     (car (cdr x)))
  (syntax (lambda-body x)    (car (cddr x)))
  (syntax (lambda-body-list x)    (cddr x))
  (syntax (lambda-nargs x)   (car (cdddr x)))
  (syntax (lambda-label x)   (car (cdr (cdddr x))))
  (syntax (lambda-debug x)   (car (cddr (cdddr x))))
  (syntax (lambda-closed? x) (car (cdddr (cdddr x))))

  (syntax (set-lambda-body x val)    (set-car! (cddr x)          val))
  (syntax (set-lambda-nargs x val)   (set-car! (cdddr x)         val))
  (syntax (set-lambda-label x val)   (set-car! (cdr (cdddr x))   val))
  (syntax (set-lambda-debug x val)   (set-car! (cddr (cdddr x))  val))
  (syntax (set-lambda-closed? x val) (set-car! (cdddr (cdddr x)) val))

  (macro pcs-extend-lambda
    (lambda (form)
      `(let  ((x ,(cadr form)))
	 (set-cdr! (cdddr x)     ; X = ('lambda bvl body nargs)
		   (list '()     ; label
			 '()	 ; debug info
			 '()))   ; closed?
	 x)))
  )

;;;                  ------ (letrec pairs body) ------

(begin
  (syntax (letrec-pairs x)    (car (cdr x)))
  (syntax (letrec-body x)     (car (cddr x)))
  (syntax (letrec-body-list x)     (cddr x))

  (syntax (set-letrec-body x val)     (set-car! (cddr x) val))
  )

;;;                  ------ (if pred then else) ------

(begin
  (syntax (if-pred x)     (car (cdr x)))
  (syntax (if-then x)     (car (cddr x)))
  (syntax (if-else x)     (car (cdddr x)))

  (syntax (set-if-pred x val)     (set-car! (cdr x)   val))
  (syntax (set-if-then x val)     (set-car! (cddr x)  val))
  (syntax (set-if-else x val)     (set-car! (cdddr x) val))
  )

;;;                     ------ (set! id exp) ------

(begin
  (syntax (set!-id x)      (car (cdr x)))
  (syntax (set!-exp x)     (car (cddr x)))

  (syntax (set-set!-id x val)     (set-car! (cdr x)  val))
  (syntax (set-set!-exp x val)    (set-car! (cddr x) val))
  )

;;; --------------------------------------------------------------------

(define pcs-make-readable				; PCS-MAKE-READABLE
 (lambda (x)
    (letrec
;-------!
 ((pmr-exp
      (lambda (x)
	(if (atom? x)
	    x
	    (case (car x)
		  (quote         x)
		  (t             (pmr-id x))
		  (lambda        (pmr-lambda x))
		  (letrec        (pmr-letrec x))
		  (else          (mapcar pmr-exp x))))))

  (pmr-id
      (lambda (x)(cadr x)))

  (pmr-full-id
      (lambda (x)
	`(t (,(id-name x) . ,(id-number x))
	    (funargsees?= ,(id-funargsees? x))
	    (freeref?=    ,(id-freeref? x))
	    (set!?=       ,(id-set!? x))
	    (init=        ,(if (id-init x) 'lambda '())))))

  (pmr-lambda
      (lambda (x)
	`(lambda
	  ,(mapcar pmr-full-id (lambda-bvl x))
	  ,(pmr-exp (lambda-body x))
	  (label= ,(lambda-label x))
	  (closed?= ,(lambda-closed? x)))))

  (pmr-letrec
      (lambda (x)
	`(letrec
	  ,(pmr-pairs (letrec-pairs x) '())
	  ,(pmr-exp (letrec-body x)))))

  (pmr-pairs
      (lambda (old new)
	(if (null? old)
	    (reverse! new)
	    (pmr-pairs (cdr old)
		       (cons (list (pmr-full-id (caar old))
				   (pmr-exp (cadar old)))
			     new)))))

  )
 (pmr-exp x))))

;;; --------------------------------------------------------------------

;
; filename-manipulating functions
;
(define filename-sans-extension
  (lambda (file)
    (let ((period (substring-find-next-char-in-set
		    file 0 (string-length file) ".")))
      (if period
	  (substring file 0 period)
	  file))))

(define extension-sans-filename
  (lambda (file)
    (let ((period (substring-find-next-char-in-set
		    file 0 (string-length file) ".")))
      (if period
	  (substring file period (string-length file))
	  ""))))

;;; --------------------------------------------------------------------

;
; Routine to compile a form, setting timing info and intermediate (between
; compiler phases) data.
;
(define pcs
  (lambda (exp)
    (begin
      (set! pme= '())
      (set! psimp= '())
      (set! pca= '())
      (set! pcg= '())
      (set! pasm= '())
      (set! pcs-local-var-count 0)
      (set! problem exp)
      (set! pcs-error-flag #!false)
      (set! t-0 (car (ptime)))
      (set! pme=    (pme   exp ))
      (set! t-pme (car (ptime)))
      (if pcs-error-flag
	  (error "[Compilation terminated because of errors]")
	  (begin
	     (set! psimp=  (psimp pme=))
	     (set! t-psimp (car (ptime)))
	     (pca psimp=)
	     (set! t-pca (car (ptime)))
	     (set! pcg=    (pcg   psimp=))
	     (set! t-pcg (car (ptime)))
	     (set! ppeep=  (ppeep pcg=))
	     (set! t-ppeep (car (ptime)))
	     (set! pasm=   (pasm  ppeep=))
	     (set! t-pasm (car (ptime)))
	     ))
      `(Times- Total= ,(- t-pasm t-0)
	       pme= ,(- t-pme t-0)
	       psimp= ,(- t-psimp t-pme)
	       pca= ,(- t-pca t-psimp)
	       pcg= ,(- t-pcg t-pca)
	       ppeep= ,(- t-ppeep t-pcg)
	       pasm= ,(- t-pasm t-ppeep))
      )))

;
; Compiles a given expression without executing the result
;
(define pcs-compile
  (lambda (exp)
    (set! pcs-verbose-flag #!false)
    (set! pcs-binary-output #!true)
    (set! pcs-local-var-count 0)
    (set! pcs-error-flag #!false)
    (let ((t1 (pme exp)))
      (if pcs-error-flag
	  (error "[Compilation terminated because of errors.]")
	  (let ((t2 (psimp t1)))
	    (pca t2)
	    (pasm (ppeep (pcg t2))))))))


;
; Set up compile-time aliases. When encountered in a source file,
; anything assigned via compile-time-alias will be defined as
; an alias, but will not be written to the object file.
;    See pcs-compile-file in this file !!!
;
(alias compile-time-alias alias)


;
; Compiles a given file without executing (unless form is a macro, alias,
; syntax, or define-integrable) the result. Also report compilation info.
;
(define pcs-compile-file
  (lambda (filename1 filename2)
    (if (or (not (string? filename1))
            (not (string? filename2))
            (equal? filename1 filename2))
        (error "PCS-COMPILE-FILE arguments must be distinct file names"
	       filename1
	       filename2)
        (fluid-let ((input-port (open-input-file filename1)))
	   (let ((o-port (open-output-file filename2)))
	     (letrec
	      ((loop
	        (lambda (form)
		  (if (eof-object? form)
		      (begin (close-input-port (fluid input-port))
			     (close-output-port o-port)
			     'ok)
		      (begin (compile-to-file form)
			     (set! form '())		; for GC
			     (loop (read))))))
	       (compile-to-file
	        (lambda (form)
		  (let* ((cform (pcs-compile form))
			 (nconstants (cadr cform))
		   	 (nbytes (caddr cform))
			 (name?? (car (cadddr cform))))
		    (if (pair? form)
		      (if (eq? (car form) 'COMPILE-TIME-ALIAS)
			(%execute cform)
		      ;else	
			(begin
			  (when (and (pair? form)
			             (memq (car form)
				           '(MACRO SYNTAX ALIAS 
					     DEFINE-INTEGRABLE)))
			     (%execute cform))
		          (writeln "      " name?? ": ("
			           nconstants "," nbytes ")")
		          (fluid-let ((output-port o-port))
		             (set-line-length! 74)		; was 120 !!
		             (prin1 `(%execute (quote ,cform)))
		             (newline)))))))))
	   (loop (read))))))))
;
; Compile object code to file. The code generated by ppeep (the peephole
; optimizer is written to the specified file.
;
;
(define %compile-file
  (lambda (filename1 filename2)
    (if (or (not (string? filename1))
            (not (string? filename2))
            (equal? filename1 filename2))
        (error "%COMPILE-FILE arguments must be distinct file names"
	       filename1
	       filename2)
        (fluid-let ((input-port (open-input-file filename1)))
           (let ((o-port (open-output-file filename2)))
	     (letrec
	      ((loop
	        (lambda (form)
		  (if (eof-object? form)
		      (begin (close-input-port (fluid input-port))
			     (close-output-port o-port)
			     'ok)
		      (begin (compile-to-file form)
			     (set! form '())		; for GC
			     (loop (read))))))
	       (compile-to-file
	        (lambda (form)
		  (let ((t1 (pme form)))
		    (if pcs-error-flag
			(writeln "[Compilation terminated because of errors.]")
			(let ((t2 (psimp t1)))
			  (pca t2)
			  (set! ppeep= (ppeep (pcg t2))))))
		  (fluid-let ((output-port o-port))
		     (set-line-length! 74)		; was 120 !!
		     (newline)
		     (pp form)
		     (newline)
		     (pcs-princode ppeep=)
		     (newline)))))
	      (loop (read))))))))

