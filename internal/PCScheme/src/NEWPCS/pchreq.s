
;      -*- Mode: Lisp -*-			      Filename:  pchreq.s

;                     Last Revision:  3-Sep-85 1500ct

;--------------------------------------------------------------------------;
;									   ;
;                         TI SCHEME -- PCS Compiler                        ;
;                  Copyright 1985 (c) Texas Instruments                    ;
;									   ;
;			       David Bartley				   ;
;									   ;
;		      Character and String Operations			   ;
;									   ;
;--------------------------------------------------------------------------;


;;;; The operations defined here are those proposed by Chris Hanson on
;;;;  14 Jan 1985 and in a revision on 20 Mar 85.


;;;; ------------------- Basic Character Operations --------------------

;;; CHAR?			 PCS primitive (opcode)
;;; CHAR=?			 PCS primitive (opcode)
;;; CHAR-CI=? 			 PCS primitive (opcode)
;;; CHAR<?			 PCS primitive (opcode)
;;; CHAR-CI<?			 PCS primitive (opcode)
;;; CHAR-UPCASE 		 PCS primitive (opcode)
;;; CHAR-DOWNCASE		 PCS primitive (opcode)
;;; CHAR->INTEGER		 PCS primitive (opcode)

(define-integrable char<=?
  (lambda (ch1 ch2)
    (or (char<? ch1 ch2)
        (char=? ch1 ch2))))

(define-integrable char>=?
  (lambda (ch1 ch2)
    (not (char<? ch1 ch2))))

(define-integrable char>?
  (lambda (ch1 ch2)
    (not (or (char<? ch1 ch2)
             (char=? ch1 ch2)))))

(define-integrable char-ci<=?
  (lambda (ch1 ch2)
    (or (char-ci<? ch1 ch2)
        (char-ci=? ch1 ch2))))

(define-integrable char-ci>=?
  (lambda (ch1 ch2)
    (not (char-ci<? ch1 ch2))))

(define-integrable char-ci>?
  (lambda (ch1 ch2)
    (not (or (char-ci<? ch1 ch2)
             (char-ci=? ch1 ch2)))))

;;;; --------------------- Basic String Operations ---------------------


;;; STRING?			 PCS primitive (opcode)
;;; STRING-LENGTH		 PCS primitive (opcode)
;;; STRING-REF			 PCS primitive (opcode)
;;; STRING-SET! 		 PCS primitive (opcode)
;;; STRING->SYMBOL		 PCS primitive (opcode)
;;; STRING->UNINTERNED-SYMBOL	 PCS primitive (opcode)
;;; SYMBOL->STRING		 PCS primitive (opcode)


;;;; ----------------------- Standard Operations -----------------------


;;; MAKE-STRING 		 PCS primitive (opcode)
;;; STRING-FILL!		 PCS primitive (opcode)
;;; SUBSTRING			 PCS primitive (opcode)


(define (string-null? string)				; STRING-NULL?
  (and (string? string)
       (zero? (string-length string))))


(define string-append					; STRING-APPEND
  (letrec
   ((sa*
     (lambda (s1 s2 rest)
       (if (null? rest)
	   (sa3 s1 '() s2)
	   (let ((s3 (car rest))
		 (rest (cdr rest)))
	     (if (null? rest)
		 (sa3 s1 s2 s3)
		 (sa3 s1 s2 (sa* s3 (car rest)(cdr rest))))))))
    (sa3
     (lambda (s1 s2 s3)
       (%string-append s1 0 (string-length s1)
		       s2
		       s3 0 (string-length s3)))))
   (lambda args
     (cond ((null? args) "")
	   ((null? (cdr args)) (car args))
	   (else (sa* (car args)(cadr args)(cddr args)))))))


(define string-copy					; STRING-COPY
  (lambda (string)
    (%string-append string 0 (string-length string)
		    '()
		    "" 0 0)))


(define string->list					; STRING->LIST
  (lambda (string)
    (do ((string string
		 string)
	 (index  0
		 (add1 index))
	 (end	 (string-length string)
		 end)
	 (result '()
		 (cons (string-ref string index) result)))
	((= index end)
	 (reverse! result)))))


(define (list->string chars)				; LIST->STRING
  (do ((chars  chars
	       (cdr chars))
       (index  0
	       (add1 index))
       (result (make-string (length chars) '())
	       result))
      ((null? chars) result)
    (string-set! result index (car chars))))


;;;; ------------------------ Motion Primitives ------------------------


(define (substring-fill! string start end char)		; SUBSTRING-FILL!
  (when (< start end)
	(string-set! string start char)
	(substring-fill! string (1+ start) end char)))


(define							; SUBSTRING-MOVE-LEFT!
  (substring-move-left! string1 start1 end1 string2 start2)
  (when (< start1 end1)
	(string-set! string2 start2
		     (string-ref string1 start1))
	(substring-move-left!
	    string1 (1+ start1) end1 string2 (1+ start2))))


(define substring-move-right!			   ; SUBSTRING-MOVE-RIGHT!
  (lambda (string1 start1 end1 string2 start2)
    (letrec ((loop
	       (lambda (count1 count2)
		 (when (<= start1 count1)
		       (string-set! string2 count2
				    (string-ref string1 count1))
		       (loop (-1+ count1) (-1+ count2)))))
	      (end2 (+ start2 (- end1 start1)))
	      )
       (loop (-1+ end1) (-1+ end2)))))


;;;; ---------------------- Comparison Primitives ----------------------


(define string=?					; STRING=?
  (lambda (s1 s2)
    (and (string? s1)(string? s2)(eqv? s1 s2))))


(define string<?					; STRING<?
  (lambda (s1 s2)
    (let loop ((s1 s1)
	       (s2 s2)
	       (i1 0)
	       (i2 0)
	       (e1 (string-length s1))
	       (e2 (string-length s2)))
	 (cond ((= i1 e1) (< e1 e2))
	       ((= i2 e2) #!false)
	       (t
		(let ((c1 (string-ref s1 i1))
		      (c2 (string-ref s2 i2)))
		  (if (char=? c1 c2)
		      (loop s1 s2 (add1 i1)(add1 i2) e1 e2)
		      (char<? c1 c2))))))))


(define string<=?				       ; STRING<=?
  (lambda (s1 s2)
    (let loop ((s1 s1)
	       (s2 s2)
	       (i1 0)
	       (i2 0)
	       (e1 (string-length s1))
	       (e2 (string-length s2)))
	 (cond ((= i1 e1) (<= e1 e2))
	       ((= i2 e2) #!false)
	       (t
		(let ((c1 (string-ref s1 i1))
		      (c2 (string-ref s2 i2)))
		  (if (char=? c1 c2)
		      (loop s1 s2 (add1 i1)(add1 i2) e1 e2)
		      (char<? c1 c2))))))))


(define string>=?				       ; STRING>=?
  (lambda (s1 s2)
    (not (string<? s1 s2))))


(define string>?				       ; STRING>?
  (lambda (s1 s2)
    (not (string<=? s1 s2))))


(define substring=?)					; SUBSTRING=?
(define substring-ci=?)					; SUBSTRING-CI=?

(letrec
  ((make-substring=
    (lambda (char-test)
      (lambda (string1 start1 end1 string2 start2 end2)
	(define (loop index1 index2)
	  (or (= index1 end1)
	      (and (char-test (string-ref string1 index1)
			      (string-ref string2 index2))
		   (loop (1+ index1) (1+ index2)))))
	(and (string? string1)
	     (string? string2)
	     (= (- end1 start1) (- end2 start2))
	     (loop start1 start2))))))
  (begin
    (set! substring=?					; SUBSTRING=?
	  (make-substring= (lambda (a b)(char=? a b))))
    (set! substring-ci=?				; SUBSTRING-CI=?
	  (make-substring= (lambda (a b)(char-ci=? a b))))))


(define substring<?)					; SUBSTRING<?
(define substring-ci<?)					; SUBSTRING-CI<?

(letrec
 ((make-substring<
    (lambda (char=test char<test)
      (lambda (string1 start1 end1 string2 start2 end2)
	(letrec ((loop
		  (lambda (index1 index2)
		    (cond ((or (= index1 end1)
			       (= index2 end2))
			   (< (- end1 start1)
			      (- end2 start2)))
			  ((char=test (string-ref string1 index1)
				      (string-ref string2 index2))
			   (loop (1+ index1) (1+ index2)))
			  (else
			   (char<test (string-ref string1 index1)
				      (string-ref string2 index2)))))))
		(and (string? string1)
		     (string? string2)
		     (loop start1 start2)))))))
 (begin
   (set! substring<?					; SUBSTRING<?
	 (make-substring<
	        (lambda (a b)(char=? a b))
		(lambda (a b)(char<? a b))))
   (set! substring-ci<?					; SUBSTRING-CI<?
	 (make-substring<
	        (lambda (a b)(char-ci=? a b))
		(lambda (a b)(char-ci<? a b))))))


(define string-ci=?)					; STRING-CI=?
(define string-ci<?)					; STRING-CI<?

(letrec
  ((string-comparison
    (lambda (substring-comparison)
      (lambda (string1 string2)
	(substring-comparison string1 0 (string-length string1)
			      string2 0 (string-length string2))))))
  (begin
    (set! string-ci=?					; STRING-CI=?
	  (string-comparison substring-ci=?))
    (set! string-ci<?					; STRING-CI<?
	  (string-comparison substring-ci<?))))
