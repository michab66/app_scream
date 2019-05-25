
;    -*- Mode: Scheme -*-                             Filename:  psort.s

;                     Last Revision:  15-Jan-87 0900ct

;--------------------------------------------------------------------------;
;                                                                          ;
;                         TI SCHEME -- PCS Runtime                         ;
;		 (c) Copyright 1987 by Texas Instruments                   ;
;                                                                          ;
;			      David Bartley                                ;
;                                                                          ;
;	    Destructive SORT! routines for Vectors and Lists               ;
;                                                                          ;
;--------------------------------------------------------------------------;

;; MERGE-SORT!  is adapted from an algorithm contributed to TI by Dr
;; Alexander Stepanov of Polytechnic Institute of New York CS Dept, 30
;; October 1985.  Tests show it to take 60% of the time of the old PC
;; Scheme SORT!  for lists.  It is also faster than two different imple-
;; mentations of Quicksort, so we use it to sort both vectors and lists.

;; (Performance figures given above are based on timings using PC Scheme
;; and should be remeasured for other implementations.)

(define (sort! obj . rest)
  (letrec
    ((merge-sort!				; merge-sort! (for lists)
       (lambda (L less?)
	 (listify! L)
	 (par-reduce less? L)))

     (listify!
       (lambda (L)
         (when (pair? L)
               (set-car! L (cons (car L) '()))
               (listify! (cdr L)))))

     (merge!
       (lambda (less? L1 L2)
         (if (less? (car L1) (car L2))
             (merge-tail less? (cdr L1) L2 L1 L1)
             (merge-tail less? L1 (cdr L2) L2 L2))))

     (merge-tail
       (lambda (less? L1 L2 result last)
         (cond ((null? L1)
                (set-cdr! last L2)
                result)
               ((null? L2)
                (set-cdr! last L1)
                result)
               ((less? (car L1) (car L2))
                (set-cdr! last L1)
                (merge-tail less? (cdr L1) L2 result L1))
               (else
                (set-cdr! last L2)
                (merge-tail less? L1 (cdr L2) result L2)))))

     (par-reduce
       (lambda (less? list)
         (if (null? (cdr list))
             (car list)
             (par-reduce less? (step-reduce less? list list)))))

     (step-reduce
       (lambda (less? list L)
         (if (null? (cdr L))
             list
             (let ((next (cddr L)))
               (set-car! L (merge! less? (car L)(cadr L)))
               (set-cdr! L next)
               (step-reduce less? list next)))))
     )
    (let ((less? (or (and rest (car rest))
		     %sort-less?)))
      (cond ((vector? obj)     (list->vector (merge-sort! (vector->list obj) less?)))
            ((null? obj)       obj)
            ((not (pair? obj)) (%error-invalid-operand 'SORT! obj))
            ((null? (cdr obj)) obj)
            (else	       (merge-sort! obj less?))))))

;; number < char < string < symbol < list < vector

(define %sort-less?					; %SORT-LESS?
  (letrec
   ((type-of
     (lambda (obj)
       (cond ((or (null? obj) (pair? obj)) 4)
	     ((symbol? obj) 3)
	     ((vector? obj) 5)
	     ((string? obj) 2)
	     ((char? obj) 1)
	     ((number? obj) 0)
	     (else 42))))
    (symbol-less
     (lambda (obj1 obj2)
       (string<? (symbol->string obj1)(symbol->string obj2))))
    (list-less
     (lambda (obj1 obj2)
       (cond ((null? obj2) #!false)
	     ((null? obj1) #!true)
	     ((less (car obj1)(car obj2)) #!true)
	     ((less (car obj2) (car obj1)) #!false)
	     (else (less (cdr obj1) (cdr obj2))))))
    (vector-less
     (lambda (v1 v2)
       (let ((l1 (vector-length v1))
	     (l2 (vector-length v2)))
	 (let loop ((i1 0)(i2 0))
	      (cond ((= i2 l2) #!false)
		    ((= i1 l1) #!true)
		    ((less (vector-ref v1 i1) (vector-ref v2 i2))
		     #!true)
		    ((less (vector-ref v2 i2) (vector-ref v1 i1))
		     #!false)
		    (else
		     (loop (add1 i1) (add1 i2))))))))
    (less
     (lambda (obj1 obj2)
       (let ((t1 (type-of obj1))
	     (t2 (type-of obj2)))
	 (cond ((< t1 t2) #!true)
	       ((> t1 t2) #!false)
	       (else (case t1
		       ((0) (< obj1 obj2))
		       ((1) (char<? obj1 obj2))
		       ((2) (string<? obj1 obj2))
		       ((3) (symbol-less obj1 obj2))
		       ((4) (list-less obj1 obj2))
		       ((5) (vector-less obj1 obj2))
		       (else #!true))))))))
   (lambda (obj1 obj2)
     (less obj1 obj2))))
