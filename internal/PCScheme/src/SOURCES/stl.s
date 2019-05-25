;;; PC Scheme toplevel
;;; Copyright 1987 (c) Texas Instruments


;;; The following is the PC Scheme standard toplevel function.
;;; This definition of it is suitable for loading via an .INI file.


; When this is loaded, the fluid variable SCHEME-TOP-LEVEL is set
; to the outer lambda expression.  When PC Scheme finishes loading
; the .INI file, it does an internal SCHEME-RESET.  That activates
; this function, and also snapshots the VM state; further SCHEME-RESET's
; will always restore the state of PC Scheme to this initial snapshot.
; The outer lambda expression's body calls the local function ==SCHEME-RESET==.
; The fluid variables INPUT-PORT and OUTPUT-PORT are initialized to the
; values of STANDARD-INPUT and STANDARD-OUTPUT, which in turn are always
; bound to 'CONSOLE unless you explicitly set them otherwise.
; The history list is set to nil.  The debug-mode flag is examined and
; an appropriate message is output.  Then comes the most interesting
; part--a continuation snapshots the context at this point of execution
; in the function and is assigned to the variable ==RESET==.  Then the
; fluid variable SCHEME-TOP-LEVEL is rebound to this continuation.
; Henceforth, further RESET's will start execution of the toplevel function
; at this point, skipping the above initializations.  A GC is done before
; executing the local function MORE.

; MORE is the read-eval-print section of the toplevel.	The prompt is
; displayed.  Input is read, consed onto the history list, and evaluated,
; with the result printed with WRITE and also consed onto the history list.
; In the midst of this, the local variable NEXT is bound to SCHEME-TOP-LEVEL's
; value.  It is possible that the evaluation of the input form might have
; changed SCHEME-TOP-LEVEL.  If NEXT is still bound to ==RESET==, the
; continuation derived above, then the current toplevel function is still
; in control and we loop back to MORE, skipping the initializations that
; RESET or SCHEME-RESET would perform.	Otherwise, a new toplevel is
; indicated, and we call it.

; To summarize, the system's toplevel function has 3 entry points.
; First, SCHEME-RESET restarts the outer lambda expression,
; which invokes the local function ==SCHEME-RESET==, and that
; resets the history list and input and output ports, among other things.
; Second, RESET restarts the continuation marked by the CALL/CC,
; and a GC occurs.  Finally, the local function MORE takes care
; of the read-eval-print loop.	Once entered, MORE is never exited
; unless a RESET or SCHEME-RESET are executed to redo their appropriate
; levels of initialization.


;;; define standard toplevel loop and support functions


(set! (fluid scheme-top-level)
  (lambda ()					; outer lambda
    (letrec
      ((==reset== '())
       (==scheme-reset==			; here for SCHEME-RESET
	 (lambda ()
	   (set! (fluid input-port) standard-input)
	   (set! (fluid output-port) standard-output)
	   (putprop '%PCS-STL-HISTORY (list '()) %pcs-stl-history)
	   (newline)
	   (display "[PCS-DEBUG-MODE is ")
	   (display (if pcs-debug-mode "ON]" "OFF]"))
	   (newline)
	   (call/cc
	     (lambda (k)
	       (set! ==reset== (lambda () (k '())))
	       (set! (fluid scheme-top-level)
		     ==reset==)))
						; here for RESET
	   (gc)
	   (more)))
       (more					; read-eval-print loop
	 (lambda ()
	   (fresh-line)
	   (display "[")
	   (display (length (getprop '%PCS-STL-HISTORY %pcs-stl-history)))
	   (display "]> ")
	   (let ((problem (read)))
	     (flush-input)
	     (if (eof-object? problem)
		 (display "[End of file read by SCHEME-TOP-LEVEL]")
		 (begin
		   (putprop '%PCS-STL-HISTORY
			    (cons (list problem)
				  (getprop '%PCS-STL-HISTORY
					   %pcs-stl-history))
			    %pcs-stl-history)
		   (let* ((answer (eval problem))
			  (next (fluid scheme-top-level)))
		     (when (not (eq? answer *the-non-printing-object*))
			   (write answer))
		     (putprop '%PCS-STL-HISTORY
			      (cons (cons problem answer)
				    (cdr (getprop '%PCS-STL-HISTORY
						  %pcs-stl-history)))
			      %pcs-stl-history)
		     (if (eq? next ==reset==)
			 (more)
			 (next))))))))) ;end of letrec vars
      (==scheme-reset==)			;letrec body
      )))

  ;;; %C accesses the nth user command
  ;;; %D accesses the result of the nth user command

(define %c						; %C
  (lambda (n)
    (let ((history (getprop '%PCS-STL-HISTORY %pcs-stl-history)))
      (and (positive? n)
	   (< n (length history))
	   (car (list-ref (reverse history) n))))))

(define %d						; %D
  (lambda (n)
    (let ((history (getprop '%PCS-STL-HISTORY %pcs-stl-history)))
      (and (positive? n)
	   (< n (length history))
	   (cdr (list-ref (reverse history) n))))))
