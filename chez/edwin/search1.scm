;;;
;;;	Copyright (c) 1985 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;     Modified by Texas Instruments Inc 8/15/85
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Searches

;;; **** For the time being all search and match operations are case
;;; insensitive.  This needs to be fixed later.  Also, the code has
;;; been bummed to know that strings are implemented as vectors of
;;; ASCII, and that char-sets are implemented as vectors of numbers.

;;;; Character Search

(define (make-find-next-char substring-find-next-char)
  (lambda (start end char)
    (let ((start-line (mark-line start))
	  (end-line (mark-line end)))
      (define (loop line)
	(if (eq? line end-line)
	    (let ((index
		   (substring-find-next-char (line-string line)
					     0
					     (mark-position end)
					     char)))
	      (and index (make-mark line index)))
	    (or (let ((index
		       (substring-find-next-char (line-string line)
						 0
						 (line-length line)
						 char)))
		  (and index (make-mark line index)))
		(loop (line-next line)))))
      (cond ((char=? #\newline char)
	     (and (not (eq? start-line end-line))
		  (make-mark start-line (line-length start-line))))
	    ((eq? start-line end-line)
	     (let ((index
		    (substring-find-next-char (line-string start-line)
					      (mark-position start)
					      (mark-position end)
					      char)))
	       (and index (make-mark start-line index))))
	    (else
	     (or (let ((index
			(substring-find-next-char (line-string start-line)
						  (mark-position start)
						  (line-length start-line)
						  char)))
		   (and index (make-mark start-line index)))
		 (loop (line-next start-line))))))))

(define find-next-char
  (make-find-next-char substring-find-next-char-ci))

(define (find-next-newline start end)
  (and (not (eq? (mark-line start) (mark-line end)))
       (make-mark (mark-line start) (line-length (mark-line start)))))

(define (make-find-previous-char substring-find-previous-char)
  (lambda (start end char)
    ;; Here START must come after END in the mark ordering.
    ;; The search begins at START and proceeds back until END.
    (let ((start-line (mark-line start))
	  (end-line (mark-line end)))
      (define (loop line)
	(if (eq? line end-line)
	    (let ((index
		   (substring-find-previous-char (line-string line)
						 (mark-position end)
						 (line-length line)
						 char)))
	      (and index (make-mark line (1+ index))))
	    (let ((index
		   (substring-find-previous-char (line-string line)
						 0
						 (line-length line)
						 char)))
	      (if index
		  (make-mark line (1+ index))
		  (loop (line-previous line))))))
      (cond ((char=? #\newline char))
	    ((eq? start-line end-line)
		(let ((index
		       (substring-find-previous-char (line-string start-line)
						     (mark-position end)
						     (mark-position start)
						     char)))
		  (and index (make-mark start-line (1+ index)))))
	    (else
	     (let ((index
		    (substring-find-previous-char (line-string start-line)
						  0
						  (mark-position start)
						  char)))
	       (if index
		   (make-mark start-line (1+ index))
		   (loop (line-previous start-line)))))))))

(define find-previous-char
  (make-find-previous-char substring-find-previous-char-ci))

(define (find-previous-newline start end)
  (and (not (eq? (mark-line start) (mark-line end)))
       (make-mark (mark-line start) 0)))

;;;; Character-set Search

(define ((char-set-forward-search char-set) start end limit?)
  (or (find-next-char-in-set start end char-set)
      (limit-mark-motion limit? end)))

(define ((char-set-backward-search char-set) start end limit?)
  (or (find-previous-char-in-set start end char-set)
      (limit-mark-motion limit? end)))

(define (find-next-char-in-set start end char-set)
  (let ((line (mark-line start))
	(position (mark-position start))
	(end-line (mark-line end))
        (char-set-length (string-length char-set)))
    (define (loop line)
      (if (eq? line end-line)
	  (let ((index
		 (substring-find-next-char-in-set (line-string line)
						  0
						  (mark-position end)
						  char-set)))
	    (and index (make-mark line index)))
	  (or (let ((index
		     (substring-find-next-char-in-set (line-string line)
						      0
						      (line-length line)
						      char-set)))
		(and index (make-mark line index)))
	      (loop (line-next line)))))
    (if (eq? line end-line)
	(let ((index
	       (substring-find-next-char-in-set (line-string line)
						position
						(mark-position end)
						char-set)))
	  (and index (make-mark line index)))
	(or (let ((index
		   (substring-find-next-char-in-set (line-string line)
						    position
						    (line-length line)
						    char-set)))
	      (and index (make-mark line index)))
;;;	    (if (char-set-member? char-set #\Newline)
            (if (substring-find-next-char-in-set char-set 0 char-set-length
                                                 #\newline)
		(make-mark line (line-length line))
		(loop (line-next line)))))))

(define (find-previous-char-in-set start end char-set)
  ;; Here START must come after END in the mark ordering.
  ;; The search begins at START and proceeds back until END.
  (let ((line (mark-line start))
	(position (mark-position start))
	(end-line (mark-line end))
        (char-set-length (string-length char-set)))
    (define (loop line)
      (if (eq? line end-line)
	  (let ((index
		 (substring-find-previous-char-in-set (line-string line)
						      (mark-position end)
						      (line-length line)
						      char-set)))
	    (and index (make-mark line (1+ index))))
	  (or (let ((index
		     (substring-find-previous-char-in-set (line-string line)
							  0
							  (line-length line)
							  char-set)))
		(and index (make-mark line (1+ index))))
	   (loop (line-previous line)))))
    (if (eq? line end-line)
	(let ((index
	       (substring-find-previous-char-in-set (line-string line)
						    (mark-position end)
						    position
						    char-set)))
	  (and index (make-mark line (1+ index))))
	(or (let ((index
		   (substring-find-previous-char-in-set (line-string line)
							0
							position
							char-set)))
	      (and index (make-mark line (1+ index))))
;;;	    (if (char-set-member? char-set #\Newline)
            (if (substring-find-next-char-in-set char-set 0 char-set-length
                                                 #\newline)
		(make-mark line 0)
		(loop (line-previous line)))))))


;;;; String Search

(define (find-next-string start-mark end-mark string)
  (find-next-substring start-mark end-mark
		       string 0 (string-length string)))

(define (find-next-substring start-mark end-mark
			     string start end)
  (if (= start end)
      start-mark
      (let ((start-bound (mark- end-mark (-1+ (- end start)) #!false)))
  	(define (find-first mark)
  	  (let ((first-char (find-next-char mark start-bound
  					    (string-ref string start))))
	    (and first-char
		 (if (match-next-substring first-char end-mark
					   string start end)
		     first-char
		     (find-first (mark1+ first-char #!false))))))
	(and start-bound
	     (mark< start-mark start-bound)
	     (find-first start-mark)))))

(define (find-previous-string start-mark end-mark string)
  (find-previous-substring start-mark end-mark
			   string 0 (string-length string)))

(define (find-previous-substring start-mark end-mark
				 string start end)
  (if (= start end)
      start-mark
      (let ((start-bound (mark+ end-mark (-1+ (- end start)) #!false)))
        (define (find-first mark)
	  (let ((first-char
	  (find-previous-char mark start-bound
				     (string-ref string (-1+ end)))))
	    (and first-char
	         (if (match-previous-substring first-char end-mark
					       string start end)
		     first-char
		     (find-first (mark-1+ first-char #!false))))))
	(and start-bound
	     (mark> start-mark start-bound)
	     (find-first start-mark)))))
