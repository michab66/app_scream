;;;
;;;	Copyright (c) 1984 Massachusetts Institute of Technology
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

;;;; Sentences


(define char-set:sentence-terminators
  (make-char-set #\. #\? #\!))

(define find-next-sentence-terminator
  (char-set-forward-search char-set:sentence-terminators))

(define find-previous-sentence-terminator
  (char-set-backward-search char-set:sentence-terminators))

(define char-set:not-closing-chars
  (char-set-invert (make-char-set #\" #\' #\) #\])))

(define skip-next-closing-chars
  (char-set-forward-search char-set:not-closing-chars))

(define skip-next-whitespace
  (char-set-forward-search char-set:not-whitespace))


(define (forward-sentence mark n limit?)
  (cond ((positive? n) (%forward-sentence mark n limit?))
	((negative? n) (%backward-sentence mark (- n) limit?))
	(else mark)))

(define (%forward-sentence mark n limit?)
  (define (loop mark n)
    (let ((sent-end (forward-one-sentence mark)))
      (cond ((not sent-end) (limit-mark-motion limit? mark))
	    ((= n 1) sent-end)
	    (else (loop sent-end (-1+ n))))))
  (loop mark n))

(define (forward-one-sentence mark)
  (define (loop mark)
    (let ((this-line-end (line-end mark 0 #!false)))
      (or (find-next-sentence-delimiter mark this-line-end)
	  (let ((next-line-start (line-start mark 1 #!false)))
	    (if (or (not next-line-start)
		    (paragraph-terminator? next-line-start))
		(horizontal-space-start this-line-end)
		(loop next-line-start))))))
  (cond ((paragraph-delimiter? (line-start mark 0 #!false))
	 (let ((para-start (skip-next-paragraph-delimiters mark)))
	   (and para-start (loop para-start))))
	((line-end? (horizontal-space-end mark))
	 (let ((next-line-start (line-start mark 1 #!false)))
	   (and next-line-start
		(forward-one-sentence next-line-start))))
	(else (loop mark))))

(define (backward-sentence mark n limit?)
  (if (unassigned? limit?) (set! limit? #!FALSE))
  (cond ((positive? n) (%backward-sentence mark n limit?))
	((negative? n) (%forward-sentence mark (- n) limit?))
	(else mark)))

(define (%backward-sentence mark n limit?)
  (define (loop mark n)
    (let ((sent-start (backward-one-sentence mark)))
      (cond ((not sent-start) (limit-mark-motion limit? mark))
	    ((= n 1) sent-start)
	    (else (loop sent-start (-1+ n))))))
  (loop mark n))

(define (backward-one-sentence mark)
  (define (find start)
    (define (loop mark)
      (let ((this-line-start (line-start mark 0 #!false)))
	(or (find-previous-sentence-delimiter mark start this-line-start)
	    (if (paragraph-indentation? this-line-start)
		(horizontal-space-end this-line-start)
		(let ((previous-line-end (line-end mark -1 #!false)))
		  (if (or (not previous-line-end)
			  (paragraph-delimiter? previous-line-end))
		      this-line-start
		      (loop previous-line-end)))))))
    (loop start))
  (cond ((paragraph-delimiter? (line-start mark 0 #!false))
	 (let ((para-end (skip-previous-paragraph-delimiters mark)))
	   (and para-end
		(find (mark-1+ (horizontal-space-start
				(line-end para-end 0 #!false)) #!false)))))
	((line-start? (horizontal-space-start mark))
	 (let ((previous-line-end (line-end mark -1 #!false)))
	   (and previous-line-end
		(backward-one-sentence previous-line-end))))
	(else (find mark))))

(define (find-next-sentence-delimiter start end)	    
  (define (loop mark)
    (let ((sent-term (find-next-sentence-terminator mark end #!FALSE)))
      (and sent-term
	   (let ((sent-end (skip-next-closing-chars (mark1+ sent-term #!false)
						    end
						    'LIMIT)))
	     (if (sentence-end? sent-end)
		 sent-end
		 (loop sent-end))))))
  (loop start))

(define (find-previous-sentence-delimiter mark start end)
  (define (loop mark)
    (let ((sent-term (find-previous-sentence-terminator mark end #!FALSE)))
      (and sent-term
	   (let ((sent-end (skip-next-closing-chars sent-term start #!FALSE)))
	     (or (and sent-end
		      (sentence-end? sent-end)
		      (skip-next-whitespace sent-end start #!false))
		 (loop (mark-1+ sent-term #!false)))))))
  (loop mark))

(define (sentence-end? sent-end)
  (or (line-end? sent-end)
      (and (char= #\Space (mark-right-char sent-end))
	   (let ((x (mark1+ sent-end #!false)))
	     (or (line-end? x)
		 (char= #\Space (mark-right-char x)))))))


;;; Pages

;;;; Paragraphs

(define paragraph-delimiters
  (make-char-set #\.))

(define text-justifier-escape-chars
  (make-char-set #\. #\' #\- #\\ #\@))

(define (page-mark-next? mark)
  (match-next-strings mark (mark-end mark) page-delimiters))

(define (forward-paragraph mark n limit?)
  (cond ((positive? n) (%forward-paragraph mark n limit?))
	((negative? n) (%backward-paragraph mark (- n) limit?))
	(else mark)))

(define (%forward-paragraph mark n limit?)
  (define (loop mark n)
    (let ((para-end (forward-one-paragraph mark)))
      (cond ((not para-end) (limit-mark-motion limit? mark))
	    ((= n 1) para-end)
	    (else (loop para-end (-1+ n))))))
  (loop mark n))

(define (forward-one-paragraph mark)
  (conjunction (not (group-end? mark))
	       (if (paragraph-delimiter? (line-start mark 0 #!false))
		   (let ((para-start (skip-next-paragraph-delimiters mark)))
		     (conjunction para-start
				  (skip-next-paragraph-body para-start)))
		   (skip-next-paragraph-body mark))))

(define (skip-next-paragraph-delimiters mark)
  (let ((this-line-start (line-start mark 1 #!false)))
    (conjunction this-line-start
		 (if (paragraph-delimiter? this-line-start)
		     (skip-next-paragraph-delimiters this-line-start)
		     this-line-start))))

(define (skip-next-paragraph-body mark)
  (let ((this-line-start (line-start mark 1 #!false)))
    (cond ((not this-line-start) (line-end mark 0 #!false))
	  ((paragraph-terminator? this-line-start) this-line-start)
	  (else (skip-next-paragraph-body this-line-start)))))

(define (backward-paragraph mark n limit?)
  (cond ((positive? n) (%backward-paragraph mark n limit?))
	((negative? n) (%forward-paragraph mark (- n) limit?))
	(else mark)))

(define (%backward-paragraph mark n limit?)
  (define (loop mark n)
    (let ((para-start (backward-one-paragraph mark)))
      (cond ((not para-start) (limit-mark-motion limit? mark))
	    ((= n 1) para-start)
	    (else (loop para-start (-1+ n))))))
  (loop mark n))

(define (backward-one-paragraph mark)
  (conjunction
   (not (group-start? mark))
   (cond ((conjunction (line-start? mark)
		       (paragraph-indentation? mark))
	  (let ((previous-line-start (mark-1+ mark #!false)))
	    (conjunction previous-line-start
			 (backward-one-paragraph previous-line-start))))
	 ((paragraph-delimiter? (line-start mark 0 #!false))
	  (let ((para-end (skip-previous-paragraph-delimiters mark)))
	    (conjunction para-end
			 (skip-previous-paragraph-body para-end))))
	 (else
	  (skip-previous-paragraph-body (line-start mark 0 #!false))))))

(define (skip-previous-paragraph-delimiters mark)
  (let ((this-line-start (line-start mark -1 #!false)))
    (conjunction this-line-start
		 (if (paragraph-delimiter? this-line-start)
		     (skip-previous-paragraph-delimiters this-line-start)
		     this-line-start))))

(define (skip-previous-paragraph-body this-line-start)
  (cond ((paragraph-indentation? this-line-start)
	 (let ((previous-line-start (line-start this-line-start -1 #!false)))
	   (if (conjunction previous-line-start
			    (paragraph-delimiter? previous-line-start))
	       previous-line-start
	       this-line-start)))
	((paragraph-delimiter? this-line-start) this-line-start)
	(else
	 (let ((previous-line-start (line-start this-line-start -1 #!false)))
	   (if (not previous-line-start)
	       this-line-start
	       (skip-previous-paragraph-body previous-line-start))))))

 
(define (paragraph-delimiter? this-line-start)
  (disjunction
   (line-blank? this-line-start)
   (if (not *current-mode-scheme?*)
       (conjunction
	(not (group-end? this-line-start))
	(let ((char (mark-right-char this-line-start)))
	  (char-set-member? text-justifier-escape-chars char)))
       #!false)))

(define (paragraph-indentation? this-line-start)
  (and (not *current-mode-scheme?*)
       (not (line-blank? this-line-start))
       (char-blank? (mark-right-char this-line-start))))

(define (paragraph-terminator? this-line-start)
  (disjunction (paragraph-delimiter? this-line-start)
	       (paragraph-indentation? this-line-start)))


     
