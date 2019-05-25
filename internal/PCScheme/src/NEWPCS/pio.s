
;      -*- Mode: Lisp -*-			      Filename:  pio.s

;		      Last Revision: 10-Feb-87 0800ct

;--------------------------------------------------------------------------;
;									   ;
;			  TI SCHEME -- PCS Compiler			   ;
;		   Copyright 1985 (c) Texas Instruments 		   ;
;									   ;
;			       David Bartley				   ;
;									   ;
;		   Standard SCHEME Input/Output Routines		   ;
;									   ;
;	READ modified for R^3 quasi-quote			- TC	   ;
;	READ-STRING removed and coded in asm            2/10/87	- TC	   ;
;	Random I/O included from David Stevens		2/10/87	- TC	   ;
;	Fixed input-port? and output-port?    		3/13/87	- TC	   ;
;	Open-binary-input-file,open-binary-output-file	3/13/87	- TC	   ;
;	compile, etc. removed and placed in PCOMP.S			   ;
;	 for building of compiler-less system           6/02/87 - TC	   ;
;       LOAD is just defined in terms of FAST-LOAD			   ;
;	 for compilerless systems. Its real definition			   ;
;	 is in PCOMP.S.					6/15/87 - TC       ;
;       Set line-length=0 for OPEN-BINARY-OUTPUT-FILE   1/21/88 - RB       ;
;									   ;
;--------------------------------------------------------------------------;


; The following definitions are used only at compile time for readability 
; and understanding. They will not be written out to the .so file.
; See pboot.s and compile.all.

    (compile-time-alias %read-file-flag   #b00000001)	; read flag
    (compile-time-alias %write-file-flag  #b00000011)	; write flag(s)
    (compile-time-alias %window-flag      #b00000100)	; window port
    (compile-time-alias %open-file-flag   #b00001000)	; open port
    (compile-time-alias %binary-file-flag #b00100000)	; binary file
    (compile-time-alias %string-flag      #b01000000)	; string file

(define call-with-input-file				; CALL-WITH-INPUT-FILE
  (lambda (filename proc)
    (let* ((port (open-input-file filename))
	   (answer (proc port)))
      (close-input-port port)
      answer)))


(define call-with-output-file				; CALL-WITH-OUTPUT-FILE
  (lambda (filename proc)
    (let* ((port (open-output-file filename))
	   (answer (proc port)))
      (close-output-port port)
      answer)))


(define current-column					; CURRENT-COLUMN
  (lambda args
    (+ 1 (%reify-port (car args) 1))))


(define-integrable current-input-port			; CURRENT-INPUT-PORT
  (lambda ()
    (fluid input-port)))

(define-integrable current-output-port			; CURRENT-OUTPUT-PORT
  (lambda ()
    (fluid output-port)))

(define eof-object?					; EOF-OBJECT?
  (lambda (obj)
    (eqv? obj eof)))		; temporary ???


;;;
;;; Compile functions are now in PCOMP.S,               ; COMPILE
;;; which reflects compiler-only functions
;;;


(define fast-load					; FAST-LOAD
  (lambda (file)
    (letrec ((fasl
	      (lambda (name)
		(let ((pgm (%%fasl name)))
		  (when (not (eof-object? pgm))
			(%execute pgm)
			(fasl '() ))))))
	(if (string? file)
	    (if (file-exists? file)
		(begin
		  (fasl file)
		  'ok)
		(error "FAST-LOAD file does not exist" file))
	    (%error-invalid-operand 'FAST-LOAD file)))))

(if (unbound? load)					  
  (define load fast-load))				; LOAD

(define file-exists?					; FILE-EXISTS?
   (lambda (name)
     (and (string? name)
	   (not (string-null? name))
	   (call/cc
	   (fluid-lambda (*file-exists-open*)
	      (let ((port (%open-port name 'read)))
	        (if (port? port)
	          (begin
		   (close-input-port port)
		   #!true)
		;else
		  #!false)))))))

(define flush-input					; FLUSH-INPUT
  (lambda args
    (let ((x '())
	  (port (car args)))
      (if (and (not (zero? (%logand (%reify-port port 11) %open-file-flag)))
	       (zero? (%logand (%reify-port port 11) %read-file-flag))
	       (char-ready? port))
	  (do ((x (read-char port) (read-char port)) )
	      ((or (eq? x #\newline)
		   (eof-object? x)
		   (not (char-ready? port)))))))))

             

(define fresh-line					; FRESH-LINE
  (lambda p
    (when p (set! p (car p)))
    (when (positive? (%reify-port p 1))
	  (newline p))))


(define input-port?					; INPUT-PORT?
  (lambda (p)
    (and (port? p)
         (let ((pflags (%reify-port p 11)))
           (and (not (zero? (%logand %open-file-flag pflags)))
	        (zero? (%logand %read-file-flag pflags)))))))

(define line-length					; LINE-LENGTH
  (lambda args
    (%reify-port (car args) 5)))

(define open-input-file                               ; OPEN-INPUT-FILE
     (lambda (name) (%open-port name 'read)))

(define open-binary-input-file                        ; OPEN-BINARY-INPUT-FILE
     (lambda (name)
        (let ((port (%open-port name 'read)))
          (%reify-port! 
            port 
            11
            (%logior %binary-file-flag (%reify-port port 11)))
          port)))

(define open-output-file			      ; OPEN-OUTPUT-FILE
     (lambda (name) (%open-port name 'write)))

(define open-binary-output-file                       ; OPEN-BINARY-OUTPUT-FILE
     (lambda (name)
        (let ((port (%open-port name 'write)))
          (%reify-port! 
            port
            11 
            (%logior %binary-file-flag (%reify-port port 11)))
	  (set-line-length! 0 port)
          port)))

(define open-extend-file			      ; OPEN-EXTEND-FILE
     (lambda (name) (%open-port name 'append)))

(define close-input-port 			      ; CLOSE-INPUT-PORT	
     (lambda (port) (%close-port port)))

(define close-output-port			      ; CLOSE-OUTPUT-PORT
     (lambda (port) (%close-port port)))


(define (open-input-string str) 			; OPEN-INPUT-STRING
  (if (string? str)
      (let ((p (%make-window '())))
	(%reify! p 0 str)
	(%reify-port! p 2 3)
	(%reify-port! p 11 (%logior %string-flag (%reify-port p 11)))
	p)
      (%error-invalid-operand 'OPEN-INPUT-STRING str)))


(define output-port?					; OUTPUT-PORT?
  (lambda (p)
      (and (port? p)
        (let ((pflags (%reify-port p 11)))
          (and (not (zero? (%logand %open-file-flag pflags)))
	       (not (zero? (%logand %write-file-flag pflags))))))))

(define read						; READ
  (letrec
   ((rd-object
     (lambda (port qq?)
       (let ((item (read-atom port)))
	 (cond ((eof-object? item)   item)
	       ((atom? item)	     item)
	       (else
		(let ((item (car item)))
		  (case item
		    (|#(|  (rd-vector-tail port qq?))
		    ( |(|  (rd-list-tail port qq?))
		    ( |)|  (error "Unexpected `)' encountered before `('"))
		    ( |.|  (dot-warning)(rd-object port qq?))
		    ( |`|  (rd-mac port #!true item #!false))
		    ( |'|  (rd-mac port qq? item #!false))
		    ((|[| |]| |{| |}|)
			   item)
		    (else  (rd-mac port qq? item #!true)))))))))
    (rd-mac
     (lambda (port qq? item qq-op?)
       (if (and (not qq?) qq-op?)
	   (error "Invalid outside of QUASIQUOTE expression:" item)
	   (let ((obj (rd-object port qq?)))
	     (if (eof-object? obj)
		 (eof-warning)
		 (list (cdr (assq item qq-ops)) obj))))))
    (rd-vector-tail
     (lambda (port qq?)
       (list->vector (rd-tail port qq? #!false '()))))
    (rd-list-tail
     (lambda (port qq?)
       (rd-tail port qq? #!true '())))
    (rd-tail
     (lambda (port qq? dot-ok? result)
       (let ((item (read-atom port)))
	 (cond ((eof-object? item)
		(eof-warning)
		(reverse! result))
	       ((atom? item)
		(if (eq? item 'quasiquote)
		  (rd-tail port #!true dot-ok? (cons item result))
		;else
		  (rd-tail port qq? dot-ok? (cons item result))))
	       (else
		(let ((item (car item)))
		  (case item
		    ( |)|  (reverse! result))
		    ( |.|  (if (and dot-ok? (not (null? result)))
			       (rd-dotted-tail port qq? result)
			       (begin
				 (dot-warning)
				 (rd-tail port qq? dot-ok? result))))
		    (else
		     (let ((obj (case item
				  (|#(|  (rd-vector-tail port qq?))
				  ( |(|  (rd-list-tail port qq?))
				  ( |`|  (rd-mac port #!true item #!false))
				  ( |'|  (rd-mac port qq? item #!false))
				  ((|[| |]| |{| |}|)
					 item)
				  (else  (rd-mac port qq? item #!true)))))
		       (rd-tail port qq? dot-ok? (cons obj result)))))))))))
    (rd-dotted-tail
     (lambda (port qq? result)
       (let ((tail (rd-tail port qq? #!false '())))
	 (append! (reverse! result)
		  (cond ((and (pair? tail)
			      (null? (cdr tail)))
			 (car tail))
			(else
			 (dot-warning)
			 tail))))))
    (dot-warning
     (lambda ()
       (newline)
       (display "WARNING -- Invalid use of `.' encountered during READ")
       (newline)))
    (eof-warning
     (lambda ()
       (newline)
       (display "WARNING -- EOF encountered during READ")
       (newline)
       eof))
    (qq-ops
     '((|'|  . QUOTE)
       (|`|  . QUASIQUOTE)
       (|,|  . UNQUOTE)
       (|,@| . UNQUOTE-SPLICING)
       (|,.| . UNQUOTE-SPLICING!))))
   (lambda args
     (let ((port (car args)))
       (rd-object port #!false)))))

;
; READ-LINE re-coded in assembly language on 2-10-86 by TC
;
;(define read-line					; READ-LINE
; (lambda args
;   (define (readln-rec port n char char-list)
;     (cond ((eof-object? char)
;	     (if (null? char-list)
;		 char
;		 (fill-string (trim char-list))))
;	    ((eqv? char #\return)
;	     (if (null? char-list)
;		 ""
;		 (fill-string (trim char-list))))
;	    ((eqv? char #\newline)
;	     (readln-rec port n (read-char port) char-list))
;	    (else
;	     (readln-rec port (+ n 1) (read-char port)
;			 (cons char char-list)))))
;   (define (trim char-list)
;     (cond ((null? char-list)
;	     '())
;	    ((eqv? (car char-list) #\space)
;	     (trim (cdr char-list)))
;	    (else
;	     char-list)))
;   (define (fill-string char-list)
;     (let ((size (length char-list)))
;	(fill-rec char-list (- size 1) (make-string size '()))))
;   (define (fill-rec char-list i string)
;     (if (null? char-list)
;	  string
;	  (begin
;	    (string-set! string i (car char-list))
;	    (fill-rec (cdr char-list) (- i 1) string))))
;   (let ((port (and args (car args))))
;     (readln-rec port 0 (read-char port) '()))))
;

(define set-line-length!				; SET-LINE-LENGTH!
  (lambda (value . rest)
    (%reify-port! (car rest) 5 value)
    '()))


(define transcript-on)
(define transcript-off)

(let ((port '()))
  (set! transcript-on					; TRANSCRIPT-ON
    (lambda (file)
      (when (not (null? port))
	    (transcript-off))
      (cond ((string? file)
	     (set! port (open-extend-file file))
	     (if (port? port)
		 (begin
		   (%transcript port)
		   'ok )
		 (begin
		   (set! port '())
		   (error "Unable to open transcript file" file))))
	    ((window? file)
	     (set! port file)
	     (%transcript file)
	     'ok)
	    (else
	     (error "Invalid argument to transcript-on" file)))))

  (set! transcript-off					; TRANSCRIPT-OFF
    (lambda ()
      (when (not (null? port))
	    (%transcript '())
	    (close-output-port port)
	    (set! port '()))
      'ok)))


;;; WITH-INPUT-FROM-FILE and WITH-OUTPUT-TO-FILE need to be rewritten
;;; to use DYNAMIC-WIND, or its equivalent.


(define with-input-from-file				; WITH-INPUT-FROM-FILE
  (lambda (filename thunk)
    (let ((port (open-input-file filename)))
      (if (port? port)
	  (let ((ans (fluid-let ((input-port port)) (thunk))))
	    (close-input-port port)
	    ans)
	  port))))


(define with-output-to-file				; WITH-OUTPUT-TO-FILE
  (lambda (filename thunk)
    (let ((port (open-output-file filename)))
      (if (port? port)
	  (let ((ans (fluid-let ((output-port port)) (thunk))))
	    (close-output-port port)
	    ans)
	  port))))


(define window? 					; WINDOW?
  (lambda (obj)
    (and (port? obj)
	 (positive? (%logand (%reify-port obj 11) %window-flag)))))


(define writeln 					; WRITELN
  (lambda args
    (do ((args args (cdr args)))
	((null? args)
	 (newline))
      (display (car args)))))

;****************************************************************************
;* SET-FILE-POSITION will move the file pointer to a new position	    *
;* and update a pointer in the buffer to point to a new location.	    *
;* The offset variable can be:						    *
;*		       0 for positioning from the start of the file	    *
;*		       1 for positioning relative to the current position   *
;*		       2 for positioning from the end of the file	    *
;****************************************************************************

(define set-file-position!		   		; SET-FILE-POSITION! 
  (lambda (port #-of-bytes offset)
    (let ((current-pos (%reify-port port 9))
	  (end-of-buffer (%reify-port port 10))
	  (new-pos '())
	  (current-chunk (max 0 (-1+ (%reify-port port 12))))
	  (new-chunk '())
	  (messages '())
	  (file-size (+ (* (%reify-port port 4) 65536) (%reify-port port 6)))
	  (port-flags (%reify-port port 11)))
      (if (and (port? port)
	       (=? (%logand port-flags %window-flag) 0))
	  (case offset
	    ((0) ; offset from the start of the file
	     (set! #-of-bytes (abs #-of-bytes))
	     (if (=? (%logand port-flags %write-file-flag) 0)
		 (set! #-of-bytes (min #-of-bytes file-size)))
	     (set! new-chunk (truncate (/ #-of-bytes 256)))
	     (set! new-pos (- #-of-bytes (* new-chunk 256)))
	     (if (and (<? new-pos end-of-buffer)
		      (>=? new-pos 0)
		      (=? (%logand port-flags %write-file-flag) 0) ; open for reading
		      (=? new-chunk current-chunk))
		 (%reify-port! port 9 new-pos)
		 (%sfpos port new-chunk new-pos)))

	    ((1) ; offset from the current position
	     (set! new-pos (+ current-pos #-of-bytes))
	     (if (and (<? new-pos end-of-buffer)
		      (>=? new-pos 0)
		      (=? (%logand port-flags %write-file-flag) 0)) ; open for reading
		 (%reify-port! port 9 new-pos)
		 (begin
		   (set! new-pos (+ (+ current-pos (* 256 current-chunk))
				    #-of-bytes)) ; offset from the begining of the file
		   (if (and (>? new-pos file-size)
			    (=? (%logand port-flags %write-file-flag) 0))
		       (set! new-pos file-size))
		   (if (<? new-pos 0)
		       (set! new-pos 0))
		   (set! new-chunk (truncate (/ new-pos 256)))
		   (%sfpos port new-chunk (- new-pos (* new-chunk 256))))))

	    ((2) ; offset from the end of the file
	     (set! #-of-bytes (min (abs #-of-bytes) file-size))
	     (set! new-pos (- file-size (abs #-of-bytes))) ; absolute position
	     (set! new-chunk (truncate (/ new-pos 256)))
	     (set! new-pos (- new-pos (* new-chunk 256))) ; buffer position
	     (if (=? (%logand port-flags %write-file-flag) 0)
	       (if (and (<? new-pos end-of-buffer)
			(>=? new-pos 0)
			(=? new-chunk current-chunk))
		 (%reify-port! port 9 new-pos)
		 (%sfpos port new-chunk new-pos))
	       (error 
	        "SET-FILE-POSITION! - offset from EOF only valid for input files")
	       ))
	    (else (%error-invalid-operand 'SET-FILE-POSITION! offset)))
	  (%error-invalid-operand 'SET-FILE-POSITION! port)))))

;******************************************************************
;* get-file-position will return the current file position in the *
;* number of bytes from the beginning of the file.		  *
;******************************************************************

;(define get-file-position
;  (lambda (port)
;    (let (( result '())
;	  (chunk (max 1 (%reify-port port 12))))
;      (if (and (port? port)
;	       (=? (%logand (%reify-port port 11) %window-flag) 0))
;	  (set! result (+ (* 256 (-1+ chunk)) ; chunk#
;			  (%reify-port port 9)))	; current position
;	  (set! result "Needs to be a port/file object!"))
;      result)))

(define get-file-position				; GET-FILE-POSITION
  (lambda (port)
    (if (and (port? port)
             (=? (%logand (%reify-port port 11) %window-flag) 0))
        (+ (* 256 (-1+ (max 1 (%reify-port port 12)))) ; chunk#
           (%reify-port port 9))		       ; offset within chunk
        (error "Invalid argument to GET-FILE-POSITION.  Port object must represent a file." port))))
