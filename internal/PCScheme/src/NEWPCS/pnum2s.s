
;      -*- Mode: Lisp -*-			       Filename:  pnum2s.s

;                     Last Revision:  10-Feb-87 0900ct

;--------------------------------------------------------------------------;
;									   ;
;                         TI SCHEME -- PCS Compiler                        ;
;                  Copyright 1985 (c) Texas Instruments                    ;
;									   ;
;			       David Bartley				   ;
;									   ;
;           NUMBER->STRING and INTEGER->STRING Routines (Mark Meyer)       ;
;           STRING->NUMBER                              (Terry Caudill)    ;
;									   ;
;--------------------------------------------------------------------------;

; Revision History:
;
; tc  02/10/87 included string->number routine
;

(define string->number
  (lambda (string exactness radix)
    (if (not (or (eq? exactness 'E) (eq? exactness 'I)))
       (error "STRING->NUMBER: Invalid exactness specifier " exactness)	
       (let ((s-radix '())
	     (port '())
             (num '()))
	 (set! s-radix (apply-if (memq radix '(B O D X))
		         (lambda (val) (symbol->string (car val)))
			 (error "STRING->NUMBER: Invalid radix " radix)))
	(set! port (open-input-string (string-append "#" s-radix string)))
        (set! num (read port))
        (if (not (number? num))
	    (error "STRING->NUMBER: Can't convert string" 
		   (string-append "#" s-radix string)))
         (close-input-port port)
         num))))

(define number->string)
(define integer->string)

(letrec
 ((form-%%squares%%
   (lambda ()
     (mapc (lambda (x)
	     (let ((base (float (car x)))
		   (vec (cadr x)))
	       (do ((i (-1+ (vector-length vec)) (-1+ i)))
		   ((negative? i) 'OK)
		 (vector-set! vec i base)
		 (if (positive? i) (set! base (* base base))))))
	   %%squares%%)))

  (%%squares%%
   `((2 ,(make-vector 10)) (8 ,(make-vector 9))
     (10 ,(make-vector 9)) (16 ,(make-vector 8))))


  (scale
   (lambda (flo base)
     (if (null? (vector-ref (cadar %%squares%%) 0))
	 (form-%%squares%%))
     (if (zero? flo)
	 (cons flo 0)
	 (let ((small (< flo 1.))
	       (sqrvec (cadr (assq base %%squares%%))))
	   (let ((scale 0)
		 (local (if small (/ flo) flo))
		 (lim (vector-length sqrvec)))
	     (do ((i 0 (1+ i)))
		 ((= i lim) '())
	       (set! scale (* 2 scale))
	       (let ((sqr (vector-ref sqrvec i)))
		 (when (>= local sqr)
		       (set! scale (1+ scale))
		       (set! local (/ local sqr)))))
	     (when small
		   (set! scale (- scale))
		   (set! local (/ local))
		   (when (< local 1.)
			 (set! scale (-1+ scale))
			 (set! local (* local base))))
	     (cons local scale))))))

  (int->str
   (lambda (n base)
     (letrec
      ((i->s
	(lambda (n)
	  (if (zero? n)
	      ""
	      (let ((dig (remainder n base))
		    (rest (quotient n base)))
		(string-append
		    (i->s rest)
		    (make-string 1 (integer->char
				       (+ dig (if (> dig 9) 55 48))))))))))
      (cond ((negative? n)
	     (string-append "-" (int->str (- n) base)))
	    ((zero? n) (make-string 1 #\0))
	    (else (i->s n))))))

  (num->str
   (lambda (num format)
     (define bad-format
       (lambda ()
	 (error "NUMBER->STRING: Invalid format specification" format)))
     (if (not (number? num))
	 (error "NUMBER->STRING: Invalid argument" num))
     (if (atom? format) (bad-format))
     (letrec
      ((absnum (abs num))
       (sign (if (negative? num) "-" ""))
       (base 10)
       (radix "")
       (exact (integer? num))
       (exactness "")
       (result "")
       (sigfigs ())
       (factor ())
       (half-digit ())
       (highest-digit ())
       (numtype (car format))
       (formargs (cdr format))
       (numscale ())
       (numnorm ())
       (n ())
       (m ())
       (result-len ())
       (set-mods
        (lambda (l)
          (cond ((null? l) #!true)
                ((atom? l) ())
                ((not (set-mods (cdr l))) ())
                (else
		 (let ((mod (car l)))
		   (if (pair? mod)
		       (case (car mod)
			 (radix
			  (if (null? (cdr mod))
			      ()
			      (begin
			        (set! base
				      (cadr (assq (cadr mod)
						  '((B 2) (O 8)
						    (D 10) (X 16)))))
				(if base
				    (set! radix
					  (let ((express
						 (caddr mod)))
					    (cond ((or (eq? express 'E)
						       (null? express))
						   (cadr (assq base
							       '((2 "#b")
								 (8 "#o")
								 (10 "#d")
								 (16 "#x")
								 ))))
						  ((eq? express 'S)
						   "")
						  (else ())))))
				(and base radix))))
			 (exactness
			  (case (cadr mod)
			    (e (set! exactness (if exact "#E" "#I")))
			    (s (set! exactness ""))
			    (else ())))
			 (else ()))
		       ()))))))
       (argcheck
        (lambda (arg)
          (or (number? arg) (eq? arg 'H))))		; `Heuristic'
       (check-args
        (lambda (num-of-args)
          (if (case num-of-args
                (0 (set-mods formargs))
                (1
                 (set-mods
                    (if (argcheck (car formargs))
                        (begin
                          (set! n (car formargs))
                          (cdr formargs))
                        formargs)))
                (2
                 (set-mods
                    (if (argcheck (car formargs))
                        (begin
                          (set! n (car formargs))
                          (if (argcheck (cadr formargs))
                              (begin
                                (set! m (cadr formargs))
                                (cddr formargs))
                              (cdr formargs)))
                        formargs))))
              (begin
                (set! sigfigs
                      (cadr (assq base
                                  '((2 53) (8 17) (10 15) (16 13)))))
                (set! factor (float (expt base (-1+ sigfigs))))
                (set! half-digit
                      (integer->char (+ 48 (quotient base 2))))
                (set! highest-digit
                      (if (= base 16)
                          #\f
                          (integer->char (+ 48 (-1+ base)))))
                #!true)
              (bad-format))))
       (string-round
        (lambda (s place)
          (cond ((< place 1) s)
                ((<= (string-length s) place) s)
                ((char<? (string-ref s place) half-digit) s)
                (else
		 (do ((i (-1+ place) (-1+ i)))
		     ((or (negative? i)
			  (not (char=? (string-ref s i) highest-digit)))
		      (if (negative? i)
			  ()
			  (let ((c (string-ref s i)))
			    (string-set! s i
					 (if (char=? c #\9)
					     #\a
					     (integer->char
					      (1+ (char->integer c))))))))
		   (string-set! s i #\0))
		 (when (char=? (string-ref s 0) #\0)
		       (if (number? numscale)
			   (set! numscale (1+ numscale)))
		       (substring-move-right!
			   s 0 (-1+ (string-length s)) s 1)
		       (string-set! s 0 #\1))
		 s))))
       (flag-insignificants
        (lambda (s places c)
          (let ((len (string-length s)))
            (if (> len places)
                (substring-fill! s places len c))
            s)))
       (form-result
        (lambda (flo)
          (if (not (number? flo))
              (error "NUMBER->STRING: number too large for format" num))
          (set! flo (round flo))
          (when (and (member numtype '(FLO SCI))
		     (>= flo
			  (if (number? n)
			     (expt base n)
			     (* factor base))))		
		(set! numscale (1+ numscale))
		(set! flo (quotient flo base)))
          (set! result (int->str flo base))
          (set! result (string-round result sigfigs))
          (flag-insignificants
             result
             sigfigs
             (if (integer? num) #\0 #\#))))
       (set-result-len
        (lambda ()
          (set! result-len (string-length result))))
       (add-leading-zeros
        (lambda (n)
          (set-result-len)
          (set! result
                (cond ((string=? result "0") (make-string n #\0))
                      ((>= n result-len)
                       (string-append
                          (make-string (- n result-len) #\0)
                          result))
                      (else result)))))
       (insert-point
        (lambda (place)
          (set! result
                (string-append
                   (substring result 0 place)
                   "."
		   (if (and (float? num)
			    (= place result-len))
		      "0"	
                      (substring result place result-len))))))
       (scale-absnum
        (lambda ()
          (let ((x (scale absnum base)))
            (set! numscale (cdr x))
            (set! numnorm (car x)))))
       (kill-trailing-zeros
        (lambda (lim)
          (do ((i (-1+ (string-length result)) (-1+ i)))
              ((or (< i lim)
                   (not (char=? (string-ref result i) #\0)))
               (set! result (substring result 0 (1+ i))))
            '())))
       (float-integer
        (lambda ()
          (if (integer? absnum)
              (set! absnum (float absnum)))
          (if (not (number? absnum))
              (error
                 "NUMBER->STRING: integer too large for float conversion"
                 num))))
       (return-result
        (lambda ()
          (if (string=? result ".") (set! result "0."))
          (string-append radix exactness sign result))))
      (case numtype
	(int
	   (check-args 0)
	   (if (integer? absnum)
	       (set! result (int->str absnum base))
	       (form-result absnum))
	   (return-result))
	(fix
	   (check-args 1)
	   (if (null? n) (set! n sigfigs))
	   (if (or (eq? n 'H) (negative? n))
	       (bad-format))
	   (float-integer)
	   (form-result (* absnum (expt base n)))
	   (add-leading-zeros n)
	   (set-result-len)
	   (insert-point (- result-len n))
	   (return-result))
	(flo
	   (check-args 1)
	   (if (null? n) (set! n sigfigs))
	   (if (and (not (eq? n 'H)) (not (positive? n)))
	       (bad-format))
	   (float-integer)
	   (scale-absnum)
	   (if (or (>= numscale sigfigs) (< numscale -1))
	       (num->str num (cons 'SCI formargs))
	       (begin
		 (if (number? n)
		     (form-result (* numnorm (expt base (-1+ n))))
		     (begin
		       (form-result (* numnorm factor))
		       (kill-trailing-zeros (1+ numscale))))
		 (set-result-len)
		 (when (<= result-len numscale)
		       (set! result
			     (string-append result
				 (make-string
				     (- (1+ numscale) result-len) #\0)))
		       (set-result-len))
		 (insert-point (1+ numscale))
		 (return-result))))
	(sci
	   (check-args 2)
	   (if (or (eq? m 'H)
		   (and (number? m) (eq? n 'H)))
	       (bad-format))
	   (if (null? n) (set! n sigfigs))
	   (if (and (number? n) (null? m)) (set! m (-1+ n)))
	   (if (and (number? n)
		    (or (not (positive? n)) (negative? m) (< n m)))
	       (bad-format))
	   (float-integer)
	   (scale-absnum)
	   (if (number? n)
	       (begin
		 (form-result (* numnorm (expt base (-1+ n))))
		 (set! m (- n m)))
	       (begin
		 (form-result (* numnorm factor))
		 (set! m 1)
		 (kill-trailing-zeros m)))
	   (set-result-len)
	   (if (< m result-len) (insert-point m))
	   (set! result
		 (string-append
		     result
		     (if (= base 16) "L" "E")
		     (int->str (1+ (- numscale m)) 10)))
	   (return-result))
	(heur
	   (check-args 0)
	   (if (integer? absnum)
	       (num->str num (cons 'INT formargs))
	       (num->str num 
			 (list* (if (or (= absnum 0.0)
				        (and (>= absnum 1.0e-3)
					     (<  absnum 1.0e7)))
				   'FLO
				   'SCI)
				'H
				formargs))))
	(else (bad-format)))))))
 (set! number->string				; number->string
   (lambda (n f)
     (num->str n f)))
 (set! integer->string				; integer->string
   (lambda (n base)
     (int->str n base))))
