
;      -*- Mode: Lisp -*-			    Filename:  padvise.s

;                     Last Revision:  1-Oct-85 1400ct

;--------------------------------------------------------------------------;
;                                                                          ;
;                         TI SCHEME -- PCS Compiler                        ;
;                  Copyright 1985 (c) Texas Instruments                    ;
;                                                                          ;
;                              David Bartley                               ;
;                                                                          ;
;                     MIT Scheme Advisory Procedures                       ;
;                                                                          ;
;--------------------------------------------------------------------------;

(begin
  (define *args*)
  (define *proc*)
  (define *result*)
  (define advise-entry)
  (define advise-exit)
  (define break)
  (define break-both)
  (define break-entry)
  (define break-exit)
  (define trace)
  (define trace-both)
  (define trace-entry)
  (define trace-exit)
  (define unadvise)
  (define unadvise-entry)
  (define unadvise-exit)
  (define unbreak)
  (define unbreak-entry)
  (define unbreak-exit)
  (define untrace)
  (define untrace-entry)
  (define untrace-exit)
  (define %advise-info-vector-list)
  )

;;; info-vector format:
;;;
;;;	0 : LINK		next info-vector / ()		** NOT USED **
;;;	1 : WRAPPER		orig closure object with new contents
;;;	2 : WRAPPEE		new closure object with old contents
;;;	3 : ENTRY-ADVICE	list of entry procedures / ()
;;;	4 : EXIT-ADVICE		list of exit procedures / ()
;;;
;;; closure object format:
;;;
;;;    -1 : LENGTH		(indices are for use with %REIFY)
;;;	0 : DEBUG-INFO		source, name, etc
;;;	1 : ENVIRONMENT		environment object
;;;	2 : CB displacement	VM address
;;;	3 : CB offset to entry	VM fixnum
;;;	4 : NARGS		fixnum


(letrec
 (
  (*args*value    '())					; *ARGS*VALUE
  (*proc*value    '())					; *PROC*VALUE
  (*result*value  '())					; *RESULT*VALUE

  (info-vector-list '())				; INFO-VECTOR-LIST


  (add-advice						; ADD-ADVICE
   (lambda (proc advice index)
     (if (and (closure? proc)(closure? advice))
	 (let* ((info (get-info-vector proc info-vector-list))
		(advl (vector-ref info index)))
	   (when (not (memq advice advl))
		 (vector-set! info index
			      (cons advice advl)))
	   'OK)
	 (%error-invalid-operand-list 'ADVISE proc advice))))


  (get-info-vector					; GET-INFO-VECTOR
   (lambda (wrappee iv-list)
     (cond ((null? iv-list)
	    (let* ((info    (make-vector 5 '()))
		   (wrapper (make-wrapper info)))
	      (set! info-vector-list
		    (cons info info-vector-list))
	      (swap-closure-contents
	          wrapper wrappee 4)
	      (vector-set! info 1		; 1=WRAPPER
			   wrappee)		;    swap!
	      (vector-set! info 2		; 2=WRAPPEE
			   wrapper)		;    swap!
	      info))
	   ((eq? wrappee
		 (vector-ref (car iv-list) 1))	; 1=WRAPPER (not WRAPPEE)
	    (car iv-list))
	   (else
	    (get-info-vector wrappee (cdr iv-list))))))


  (swap-closure-contents				; SWAP-CLOSURE-CONTENTS
   (lambda (wrapper wrappee index)
     (if (zero? index)
	 (%reify! wrapper index		; copy the debug info
		  (%reify wrappee index))
	 (let ((value (%reify wrapper index)))
	   (%reify! wrapper index (%reify wrappee index))
	   (%reify! wrappee index value)
	   (swap-closure-contents wrapper wrappee (- index 1))))))


  (rem-advice						; REM-ADVICE
   (lambda (args	; (proc) -or- () ==> all
	    advice	; advice-proc -or- () ==> all
	    index)	; 3 -or- 4, entry/exit
     (let ((proc (car args)))
       (when (and proc (not (closure? proc)))
	     (apply %error-invalid-operand-list
		    (cons 'UNADVISE args)))
       (remove-advice proc advice index
		      info-vector-list '())
       'OK)))


  (remove-advice					; REMOVE-ADVICE
   (lambda (proc advice index iv-list new-iv-list)
     (if (null? iv-list)
	 (set! info-vector-list new-iv-list)
	 (let ((info (car iv-list)))
	   (cond ((null? proc)
		  (vector-set! info index '()))
		 ((eq? proc (vector-ref info 1))
		  (vector-set! info index
			       (if (null? advice)
				   '()
				   (delq! advice
					  (vector-ref info index))))))
	   (if (or (vector-ref info 3)
		   (vector-ref info 4))
	       (remove-advice proc advice index
			      (cdr iv-list)
			      (cons info new-iv-list))
	       (begin
		 (swap-closure-contents
		     (vector-ref info 1)	; 1=WRAPPER
		     (vector-ref info 2)	; 2=WRAPPEE
		     4)
		 (remove-advice proc advice index
				(cdr iv-list)
				new-iv-list)))))))


  (make-wrapper						; MAKE-WRAPPER
   (lambda (info-vector)
     (lambda args
       (call/cc
	 (fluid-lambda (%*BREAK*continuation)
	   (let* ((info info-vector)		; cache INFO-VECTOR
		  (proc (vector-ref info 2))	; 2=WRAPPEE
		  (env  (procedure-environment proc)))
	     (do ((advice (vector-ref info 3)	; 3=ENTRY-ADVICE
			  (cdr advice)))
		 ((null? advice))
	       ((car advice) proc args env))
	     (do ((result (apply proc args)
			  ((car advice) proc args result env))
		  (advice (vector-ref info 4)	; 4=EXIT-ADVICE
			  (cdr advice)))
		 ((null? advice)
		  result))))))))


  (print-arg-list					; PRINT-ARG-LIST
   (lambda (num args)
     (newline)
     (when args
	   (princ "  Argument ") (princ num) (princ ": ")
	   (prin1 (car args))
	   (print-arg-list (+ num 1) (cdr args)))))


  (std-break-entry					; STD-BREAK-ENTRY
   (lambda (proc args env)
     (set! *proc*value proc)
     (set! *args*value args)
     (set! *result*value '())
     (breakpoint-procedure 'BREAK-ENTRY
			   (cons proc args)
			   env
			   (%reify-stack
			       (+ (%reify-stack
				      (+ (%reify-stack -1) 6)) 6)))
     *args*value))


  (std-break-exit					; STD-BREAK-EXIT
   (lambda (proc args result env)
     (set! *proc*value proc)
     (set! *args*value args)
     (set! *result*value result)
     (breakpoint-procedure 'BREAK-EXIT
			   (list (cons proc args)
				 '|-->|
				 result)
			   env
			   (%reify-stack
			       (+ (%reify-stack
				      (+ (%reify-stack -1) 6)) 6)))
     *result*value))


  (std-trace-entry					; STD-TRACE-ENTRY
   (lambda (proc args env)
     (fresh-line)
     (princ " >>> Entering ")
     (prin1 proc)
     (print-arg-list 1 args)
     args))


  (std-trace-exit					; STD-TRACE-EXIT
   (lambda (proc args result env)
     (fresh-line)
     (princ " <<< Leaving ")
     (prin1 proc)
     (princ " with value ")
     (prin1 result)
     (print-arg-list 1 args)
     result))

  ) ; --------------------------------------------------------------
 (begin

  (set! *args*						; *ARGS*
    (lambda () *args*value))

  (set! *proc*						; *PROC*
    (lambda () *proc*value))

  (set! *result*					; *RESULT*
    (lambda () *result*value))

  (set! advise-entry					; ADVISE-ENTRY
   (lambda (proc advice)
     (add-advice proc advice 3)))

  (set! advise-exit					; ADVISE-EXIT
   (lambda (proc advice)
     (add-advice proc advice 4)))

  (set! break						; BREAK
   (lambda (proc)
     (add-advice proc std-break-entry 3)))

  (set! break-both					; BREAK-BOTH
   (lambda (proc)
     (break-entry proc)
     (break-exit proc)))

  (set! break-entry					; BREAK-ENTRY
   (lambda (proc)
     (add-advice proc std-break-entry 3)))

  (set! break-exit					; BREAK-EXIT
   (lambda (proc)
     (add-advice proc std-break-exit 4)))

  (set! trace						; TRACE
    (lambda (proc)
      (add-advice proc std-trace-entry 3)))

  (set! trace-both					; TRACE-BOTH
   (lambda (proc)
     (trace-entry proc)
     (trace-exit proc)))

  (set! trace-entry					; TRACE-ENTRY
   (lambda (proc)
     (add-advice proc std-trace-entry 3)))

  (set! trace-exit					; TRACE-EXIT
   (lambda (proc)
     (add-advice proc std-trace-exit 4)))

  (set! unadvise					; UNADVISE
    (lambda args
      (rem-advice args '() 3)
      (rem-advice args '() 4)))

  (set! unadvise-entry					; UNADVISE-ENTRY
    (lambda args
      (rem-advice args '() 3)))

  (set! unadvise-exit					; UNADVISE-EXIT
    (lambda args
      (rem-advice args '() 4)))

  (set! unbreak						; UNBREAK
    (lambda args
      (rem-advice args std-break-entry 3)
      (rem-advice args std-break-exit 4)))

  (set! unbreak-entry					; UNBREAK-ENTRY
    (lambda args
      (rem-advice args std-break-entry 3)))

  (set! unbreak-exit					; UNBREAK-EXIT
    (lambda args
      (rem-advice args std-break-exit 4)))

  (set! untrace						; UNTRACE
    (lambda args
      (rem-advice args std-trace-entry 3)
      (rem-advice args std-trace-exit 4)))

  (set! untrace-entry					; UNTRACE-ENTRY
    (lambda args
      (rem-advice args std-trace-entry 3)))

  (set! untrace-exit					; UNTRACE-EXIT
    (lambda args
      (rem-advice args std-trace-exit 4)))

  (set! %advise-info-vector-list		; for debugging ADVISE
    (lambda () info-vector-list))

  ) ; --------------------------------------------------------------
 )
