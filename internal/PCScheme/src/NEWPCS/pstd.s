
;      -*- Mode: Lisp -*-			      Filename:  pstd.s

;--------------------------------------------------------------------------;
;									   ;
;			  TI SCHEME -- PCS Compiler			   ;
;		   Copyright 1985 (c) Texas Instruments 		   ;
;									   ;
;			       David Bartley				   ;
;									   ;
;			 Standard SCHEME Routines			   ;
;									   ;
;--------------------------------------------------------------------------;
; Modification History:
;
;   tc	2/10/87 fixed implode for non-lists and lists with floats
;   tc	2/10/87 BOOLEAN? and PROCEDURE? added for R^3 Report
;   tc  6/01/87 seperated PSTD and PSTD2 for compiler-less system
;   tc  6/09/87 made list-tail a primitive operation

(begin

(define-integrable 1+				           ; 1+
       (lambda (n)(+ n 1)))

(define-integrable -1+				           ; -1+
       (lambda (n)(- n 1)))

(define-integrable add1				           ; ADD1
       (lambda (n)(+ n 1)))

(define-integrable apply			           ; APPLY
       (lambda (fn args)
	 (%apply fn args)))

(define-integrable caaaar (lambda (x) (caar (caar x))))    ; CAXXXR
(define-integrable caaadr (lambda (x) (caar (cadr x))))
(define-integrable caadar (lambda (x) (caar (cdar x))))
(define-integrable caaddr (lambda (x) (caar (cddr x))))
(define-integrable cadaar (lambda (x) (cadr (caar x))))
(define-integrable cadadr (lambda (x) (cadr (cadr x))))
(define-integrable caddar (lambda (x) (cadr (cdar x))))
;(define-integrable cadddr (lambda (x) (cadr (cddr x))))

(define-integrable call/cc 			           ; CALL/CC
       (lambda (exp)
	 (%call/cc exp)))

(define-integrable call-with-current-continuation	   ; CALL-w-c-c
       (lambda (exp)
	 (%call/cc exp)))

(define-integrable cdaaar (lambda (x) (cdar (caar x))))    ; CDXXXR
(define-integrable cdaadr (lambda (x) (cdar (cadr x))))
(define-integrable cdadar (lambda (x) (cdar (cdar x))))
(define-integrable cdaddr (lambda (x) (cdar (cddr x))))
(define-integrable cddaar (lambda (x) (cddr (caar x))))
(define-integrable cddadr (lambda (x) (cddr (cadr x))))
(define-integrable cdddar (lambda (x) (cddr (cdar x))))
(define-integrable cddddr (lambda (x) (cddr (cddr x))))

(define-integrable empty-stream?			   ; EMPTY-STREAM?
       (lambda (x)
	 (eq? x the-empty-stream)))

(define-integrable modulo				   ; MODULO
       (lambda (p q)
	 (let ((rem (remainder p q)))
	   (if (negative? (* p q))
	       (if (zero? rem)
		   rem
		   (+ rem q))
	       rem))))

(define-integrable null?				   ; NULL?
       (lambda (obj)
	 (not obj)))

(define-integrable reverse 			           ; REVERSE
       (lambda (L)
	 (reverse! (%append L '()))))

(define-integrable sub1				           ; SUB1
       (lambda (n)(- n 1)))

(define-integrable procedure?			           ; PROCEDURE?
       (lambda (obj)
	  (proc? obj)))
); end begin

(begin

(define ascii->symbol					   ; ASCII->SYMBOL
  (lambda (n)
    (string->symbol (make-string 1 (integer->char n)))))

(define (copy x)					   ; COPY
  (if (atom? x)
      x
      (cons (copy (car x))
	    (copy (cdr x)))))


(define %delay						   ; %DELAY
  (lambda (state)
    (lambda ()
      (when (closure? state)		; not yet memoized?
	    (set! state (list (state))))
      (car state))))


(define delayed-object? 				   ; DELAYED-OBJECT?
  (lambda (obj)
    (and (vector? obj)
	 (positive? (vector-length obj))
	 (eq? (vector-ref obj 0) '#!DELAYED-OBJECT))))


(define (delete! obj lst)				   ; DELETE!
  (letrec ((loop (lambda (obj a b z)
		   (cond ((atom? b)
			  z)
			 ((equal? obj (car b))
			  (set-cdr! a (cdr b))
			  (loop obj a (cdr b) z))
			 (else
			  (loop obj b (cdr b) z))))))
      (cond ((atom? lst)
	     '())
	    ((equal? obj (car lst))
	     (delete! obj (cdr lst)))
	    (else
	     (loop obj lst (cdr lst) lst)))))


(define (delq! obj lst) 				   ; DELQ!
  (letrec ((loop (lambda (obj a b z)
		   (cond ((atom? b)
			  z)
			 ((eq? obj (car b))
			  (set-cdr! a (cdr b))
			  (loop obj a (cdr b) z))
			 (else
			  (loop obj b (cdr b) z))))))
     (cond ((atom? lst)
	    '())
	   ((eq? obj (car lst))
	    (delq! obj (cdr lst)))
	   (else
	    (loop obj lst (cdr lst) lst)))))

(define %execute					   ; %EXECUTE
  (lambda (compiled-object)
    (%%execute compiled-object)))	; dangerous primitive!


(define exit						   ; EXIT
  (lambda ()
    (transcript-off)
    (%halt)
    (reset)))

(define explode 					   ; EXPLODE
  (lambda (obj)
    (let ((x (if (symbol? obj)
		 (symbol->string obj)
		 obj)))
      (cond ((string? x)
	     (do ((x	  x x)
		  (index  0
			  (add1 index))
		  (end	  (string-length x)
			  end)
		  (result '()
			  (cons (string->symbol
				   (substring x index (+ index 1)))
				result)))
		 ((= index end)
		  (reverse! result))))
	    ((integer? x)
	     (do ((n	  (abs x)
			  (quotient n 10))
		  (result '()
			  (cons (ascii->symbol (+ (remainder n 10) 48))
				result)))
		 ((< n 10)
		  (let ((result (cons (ascii->symbol (+ n 48)) result)))
		    (if (negative? x) (cons '- result) result)))))
	    (else x)))))


(define for-each					   ; FOR-EACH
  (lambda (f l)
    (do ((f f f)
	 (l l (cdr l)))
	((atom? l))
      (f (car l)))))


(define force						   ; FORCE
  (lambda (obj)
    (if (and (vector? obj)
	     (positive? (vector-length obj))
	     (eq? (vector-ref obj 0) '#!DELAYED-OBJECT))
	((vector-ref obj 1))
	(%error-invalid-operand 'FORCE obj))))


(define gc						   ; GC
  (lambda args
    ;; do NOT define with define DEFINE-INTEGRABLE !!
    ;; do NOT hoist the call to %CLEAR-REGISTERS
    (cond ((or (null? args)
	       (null? (car args)))
	   (%clear-registers)	   ; unbind the VM registers
	   (%garbage-collect))	   ; invoke the GC operation
	  (else
	   (%clear-registers)	   ; unbind the VM registers
	   (%compact-memory)))))   ; GC and compaction both


(define gcd						   ; GCD
  (lambda args
    (letrec ((gcd*
	      (lambda (args result)
		(if (null? args)
		    result
		    (gcd* (cdr args)
			  (gcd2 (abs (car args)) result)))))
	     (gcd2
	      (lambda (p q)
		(if (zero? q)
		    p
		    (gcd2 q (remainder p q))))))
	(gcd* args 0))))


(define gensym						   ; GENSYM
  (letrec
    ((counter->string
      (lambda (c n)
	(cond ((positive? c)
	       (let ((string (counter->string (quotient c 10)(+ n 1))))
		 (string-set! string
			      (- (string-length string) n 1)
			      (string-ref "0123456789" (remainder c 10)))
		 string))
	      ((zero? n)
	       "0")
	      (else
	       (make-string n '()))))))
    (let ((string "G")
	  (counter -1))
      (lambda args
	(set! counter (+ counter 1))
	(when (not (null? args))
	      (let ((arg (car args)))
		(cond ((integer? arg)
		       (set! counter (abs arg)))
		      ((string? arg)
		       (set! string arg))
		      ((symbol? arg)
		       (set! string (symbol->string arg)))
		      (else '()))))
	(string->uninterned-symbol
	 (string-append string
			(counter->string counter 0)))))))


(define head						   ; HEAD
  (lambda (stream)
    (if (and (vector? stream)
	     (positive? (vector-length stream))
	     (eq? (vector-ref stream 0) '#!STREAM))
	(vector-ref stream 1)
	(%error-invalid-operand 'HEAD stream))))

(define implode 					   ; IMPLODE
  (lambda (L)
    (cond ((null? L) '||)
	  ((atom? L)
	   (%error-invalid-operand 'implode L))
	  (else
	   (let ((n (length L)))
	     (do ((L	  L
			  (cdr L))
		  (string (make-string n '())
			  string)
		  (index  0
			  (add1 index)))
		 ((null? L)
		  (string->symbol string))
	       (let* ((x (car L)))
		 (string-set!
		     string
		     index
		     (cond ((symbol? x)
			    (string-ref (symbol->string x) 0))
			   ((string? x)
			    (string-ref x 0))
			   ((char? x)
			    x)
			   ((integer? x)
			    (integer->char x))
			   (else
			    (error "Invalid list element fot IMPLODE" x)) )))))))))


(define lcm						   ; LCM
  (letrec ((lcm*
	    (lambda (args result)
	      (if (null? args)
		  result
		  (let ((a (car args)))
		    (if (zero? a)
			0
			(lcm* (cdr args)
			      (quotient (abs (* a result))
					(gcd a result)))))))))
      (lambda args
	(lcm* args 1))))


(define (list->stream L)				   ; LIST->STREAM
  (if (null? L)
      the-empty-stream
      (let ((heapL L))	      ; control heap allocation of L
	(cons-stream (car L)
		     (list->stream (cdr heapL))))))


(define list->vector					   ; LIST->VECTOR
  (lambda (L)						   
    (let ((n (length L)))
      (do ((v (make-vector n) v)
	   (i 0 (1+ i))
	   (L L (cdr L)))
	  ((null? L) v)
	(vector-set! v i (car L))))))


(define list-ref					   ; LIST-REF
  (lambda (x n)
    (car (list-tail x n))))

;;;
;;; List-tail was re-defined as a primitive on 6-9-87
;;;
;;;(define (list-tail x n) 				   ; LIST-TAIL
;;;  (if (positive? n)
;;;      (list-tail (cdr x)(sub1 n))
;;;      x))


(define map						   ; MAP
  (lambda (f l)
    (do ((f f f)
	 (l l (cdr l))
	 (acc '() (cons (f (car l)) acc)))
	((atom? l)
	 (reverse! acc)))))


(define mapc						   ; MAPC
  for-each)


(define mapcar						   ; MAPCAR
  map)


(define random						   ; RANDOM
  (letrec ((loop
	    (lambda (r m+ m)
	      (if (> r m+)		; enough precision?
		  (remainder r m)
		  (loop (+ (* r 8192)(%random)) m+ m)))))
     (lambda (m)
       (let ((r (%random)))		; 14 bits
	 (if (and (< m 10241) (< r (- 16383 (remainder 16383 m)))) ;10 bits scaled by 10, plus 1
	     (remainder r m)
	     (loop r (* m 1024) m))))))

(define (randomize seed)				    ; RANDOMIZE
  (let ((|2^32-1| (sub1 (* 65536 65536))))
    (if (and (<= (minus |2^32-1|) seed)
	     (<= seed |2^32-1|))
	(%esc2 20 seed) 	 ;seed with the given number
	(%esc2 20 0)))) 	 ;seed derived from time of day

(define runtime 					   ; RUNTIME
  (lambda ()
    (let* ((t1 (%internal-time))
	   (hours (car t1))
	   (minutes (cadr t1))
	   (seconds (caddr t1))
	   (hundreds (cadddr t1)))
      (+ (* 100 (+ (* 60 (+ (* 60 hours)
			    minutes))
		   seconds))
	 hundreds))))


(define stream? 					   ; STREAM?
  (lambda (obj)
    (or (eq? obj the-empty-stream)
	(and (vector? obj)
	     (positive? (vector-length obj))
	     (eq? (vector-ref obj 0) '#!STREAM)))))


(define (stream->list stream)				   ; STREAM->LIST
  (if (empty-stream? stream)
      '()
      (cons (head stream)
	    (stream->list (tail stream)))))




(define symbol->ascii					   ; SYMBOL->ASCII
  (lambda (s)
    (char->integer (string-ref (symbol->string s) 0))))


(define tail						   ; TAIL
  (lambda (stream)
    (if (and (vector? stream)
	     (positive? (vector-length stream))
	     (eq? (vector-ref stream 0) '#!STREAM))
	((vector-ref stream 2))
	(%error-invalid-operand 'TAIL stream))))


(define thaw						   ; THAW
  (lambda (thunk)
    (thunk)))


(define vector->list					   ; VECTOR->LIST
  (lambda (v)
    (do ((n (vector-length v) n)
	 (i 0 (1+ i))
	 (L '() (cons (vector-ref v i) L)))
	((>= i n)
	 (reverse! L)))))

(define boolean?					   ; BOOLEAN?
   (lambda (obj)
      (or (eq? obj #T) (null? obj) #F)))

); end begin