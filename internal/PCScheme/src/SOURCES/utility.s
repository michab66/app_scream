; Utility procedures
; Copyright 1987 (c) Texas Instruments

;
;  This file contains some general utility procedures which may be
;  useful in the development of Scheme programs.


;
; FILENAME-SANS-EXTENSION - truncate any filename extension (ie ".xxx")
;			    from a given filename
;
; Example: (filename-sans-extension "e:\\dir\\file.ext") -> "e:\\dir\\file"
;
(define filename-sans-extension
  (lambda (file)
    (let ((period (substring-find-next-char-in-set
		    file 0 (string-length file) ".")))
      (if period
	  (substring file 0 period)
	  file))))

;
; EXTENSION-SANS-FILENAME - truncate any filename prefix leaving only
;			    ".xxx"
;
; Example: (extension-sans-filename "e:\\dir\\file.ext") -> ".ext"
;
(define extension-sans-filename
  (lambda (file)
    (let ((period (substring-find-next-char-in-set
		    file 0 (string-length file) ".")))
      (if period
	  (substring file period (string-length file))
	  ""))))

;
; DIRECTORY-SANS-FILENAME - truncate the filename, including any preceding
;			    \, from a given pathname.
;
; Example: (directory-sans-filename "e:\\dir\\file.ext") -> "e:\\dir"
;
(define directory-sans-filename
  (lambda (file)
    (let ((slash (substring-find-previous-char-in-set
		   file 0 (string-length file) "\\")))
      (if slash
	  (substring file 0 slash)
	  (error "Directory name missing a preceding slash." file)))))

;
; FILENAME-SANS-DIRECTORY - truncate everything to the left of the last
;			    \, including the \.
;
; Example: (filename-sans-directory "e:\\dir\\file.ext") -> "file.ext"
;
(define filename-sans-directory
  (lambda (file)
    (let ((slash (substring-find-previous-char-in-set
		   file 0 (string-length file) "\\")))
      (if slash
	  (substring file (add1 slash) (string-length file))
	  file))))

;
; DRIVE-NAME - repeatedly do directory-sans-filename until have name
;	       with no \'s.
;
; Example: (drive-name "e:\\dir\\file.ext") -> "e:"
;
(define drive-name
  (lambda (file)
    (let ((slash (substring-find-previous-char-in-set
		   file 0 (string-length file) "\\")))
      (if slash
	  (drive-name (directory-sans-filename file))
	  file))))

;
; COMPILE-FASL - This utility compiles a Scheme source file to a fasl file.
;		 Compile-fasl takes as input a source filename, and optional
;		 object and fasl filenames. If the object and/or fasl filenames
;		 are not specified, they will be created with .so and .fsl
;		 extensions respectively.
;
;     Note the use of engines to display a period, "." , during compilation.
;
; Example: (compile-fasl "file.s")    ;generates file.so and file.fsl
;

(define compile-fasl
  (lambda (src . x)
    (let ((src-nx (filename-sans-extension src)))
      (let ((obj (if (car x) (car x) (string-append src-nx ".so")))
	    (fasl (if (cadr x) (cadr x) (string-append src-nx ".fsl"))) )
	(let loop ((engine (make-engine
			     (lambda ()
			       (engine-return (compile-file src obj))))))
	  (engine 150
		  (lambda x nil)
		  (lambda (new-engine)
		    (display ".")
		    (loop new-engine))))
	(dos-call (string-append pcs-sysdir "\\make_fsl.exe")
		  (string-append obj " " fasl)
		  4095
		  1)))))

;
; COMPILE-ONLY - Compiles a given file without executing (unless form is a
;		 macro, alias, syntax, or define-integrable) the result.
;
;
; Compiles a given file without executing (unless form is a macro, alias,
; syntax, or define-integrable) the result. Also report compilation info.
;
; Example: (compile-only "file.s" "file.so")   ;generates file.so
;
(define compile-only
  (lambda (filename1 filename2)
    (if (or (not (string? filename1))
	    (not (string? filename2))
	    (equal? filename1 filename2))
       (error "COMPILE-ONLY arguments must be distinct file names"
	      filename1
	      filename2)
    ;else
       (letrec
	   ((i-port (open-input-file filename1))
	    (o-port (open-output-file filename2))
	    (loop
	      (lambda (form)
		(if (eof-object? form)
		   (begin (close-input-port i-port)
			  (close-output-port o-port)
			  'ok)
		   (begin (compile-to-file form)
			  (set! form '())               ; for GC
			  (loop (read i-port))))))
	    (compile-to-file
	      (lambda (form)
		(let ((cform (compile form)))
		  (when (and (pair? form)
			     (memq (car form)
				   '(MACRO SYNTAX ALIAS DEFINE-INTEGRABLE)))
		     (eval cform))
		  (prin1 `(%execute (quote ,cform)) o-port)
		  (newline o-port)))))

	  ; body of letrec

	  (set-line-length! 74 o-port)
	  (loop (read i-port))))))

;
; PP-LOAD - Pretty prints each form of a source file to the console
;	    as it loads that file.
;
; Example: (pp-load "file.s")
;
(define (pp-load filename)
  (define (load-form port)
    (let ((form (read port))
	  (result '()))
      (if (not (eof-object? form))
	  (begin
	    (newline)
	    (newline)
	    (pp form)
	    (set! result (eval (compile form)))
	    (if (not (eq? result *the-non-printing-object*))
		(begin (newline) (prin1 result)))
	    (load-form port)))))
  (if (not (string? filename))
      (error "Argument to PP-LOAD not a filename" filename)
      ;else
      (begin
	(load-form (open-input-file filename))
	(newline)
	'ok)))

;
; TIMER - measures the execution speed of any arbitrary Scheme expression
;	  The argument EXPR is the expression to be timed while ITER is
;	  the number of times the expression should be invoked. TIMER also
;	  takes into account the time spent in the control loop of the
;	  TIMER function itself by subtracting this from the total time;
;	  therefore, the value returned accurately reflects the time actually
;	  spent executing the expression.
;
; Example: (timer (fib 15) 10)	 ;report the time taken to execute
;				 ;(fib 15) 10 times
;

(syntax (timer expr iter)
	(let* ((start-time (runtime))
	       (end-time (do ((counter 1 (+ counter 1)))
			     ((> counter iter) (runtime))
			     ((lambda () #F))))
	       (go (begin (gc #T) (runtime)))
	       (stop (do ((counter 1 (+ counter 1)))
			 ((> counter iter) (runtime))
			 ((lambda () expr))))
	       (overhead (- end-time start-time))
	       (net-time (- (- stop go) overhead)))
	  (/ net-time 100.0)))
