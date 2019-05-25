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


;;;; String Match

(define (match-next-strings start end strings)
  (define (loop strings)
    (and (not (null? strings))
	 (or (match-next-string start end (car strings))
	     (loop (cdr strings)))))
  (loop strings))

(define (match-next-string start end string)
  (match-next-substring start end string 0 (string-length string)))

(define (substring-next-newlines string start end)
  (define (loop start)
    (let ((newline (substring-find-next-char string start end #\newline)))
      (if (not newline)
	  (list (- end start))
	  (cons newline (loop (1+ newline))))))
  (loop start))

(define (match-previous-strings start end strings)
  (define (loop strings)
    (and (not (null? strings))
	 (or (match-previous-string start end (car strings))
	     (loop (cdr strings)))))
  (loop strings))

(define (match-previous-string start end string)
  (match-previous-substring start end string 0 (string-length string)))

(define (substring-previous-newlines string start end)
  (define (loop end)
    (let ((newline
           (substring-find-previous-char string start end #\newline)))
      (if (not newline)
	  (list (- end start))
	  (cons (1+ newline) (loop newline)))))
  (loop end))

(define (match-next-substring start-mark end-mark string start end)
  (let ((newlines (substring-next-newlines string start end))
        (start-line (mark-line start-mark))
	(start-position (mark-position start-mark))
	(end-line (mark-line end-mark))
	(end-position (mark-position end-mark)))
    (define (match-rest line start newlines)
      (cond ((eq? line end-line)
	     (and (null? (cdr newlines))
		  (<= (car newlines) end-position)
		  (substring-equal-ci? string start end
				       (line-string line) 0
				       (car newlines))
		  (make-mark line (car newlines))))
	    ((null? (cdr newlines))
	     (and (<= (car newlines) (line-length line))
		  (substring-equal-ci? string start end
				       (line-string line) 0
				       (car newlines))
		  (make-mark line (car newlines))))
	    (else
	     (and (substring-equal-ci? string start (car newlines)
				       (line-string line) 0
				       (line-length line))
		  (match-rest (line-next line)
			      (1+ (car newlines))
			      (cdr newlines))))))

    (cond ((eq? start-line end-line)
	   (and (null? (cdr newlines))
		(let ((end-position* (+ start-position (car newlines))))
		  (and (<= end-position* end-position)
		       (substring-equal-ci? string start end
					    (line-string start-line)
					    start-position
					    end-position*)
		       (make-mark start-line end-position*)))))
	  ((null? (cdr newlines))
	   (let ((end-position* (+ start-position (car newlines))))
	     (and (<= end-position* (line-length start-line))
		  (substring-equal-ci? string start end
				       (line-string start-line)
				       start-position
				       end-position*)
		  (make-mark start-line end-position*))))
	  (else
	   (and (substring-equal-ci? string start (car newlines)
				     (line-string start-line)
				     start-position
				     (line-length start-line))
		(match-rest (line-next start-line)
			    (1+ (car newlines))
			    (cdr newlines)))))))

(define (match-previous-substring start-mark end-mark string start end)
  ;; Here START-MARK must come after END-MARK in the mark ordering.
  ;; The match begins at START-MARK and proceeds back until END-MARK.
  (let ((newlines (substring-previous-newlines string start end))
        (start-line (mark-line start-mark))
	(start-position (mark-position start-mark))
	(end-line (mark-line end-mark))
	(end-position (mark-position end-mark)))
    (define (match-rest line end newlines)
      (cond ((eq? line end-line)
	     (and (null? (cdr newlines))
		  (<= end-position (car newlines))
		  (substring-equal-ci? string start end
				       (line-string line) (car newlines)
				       (line-length line))
		  (make-mark line (car newlines))))
	    ((null? (cdr newlines))
	     (and (<= 0 (car newlines))
		  (substring-equal-ci? string start end
				       (line-string line) (car newlines)
				       (line-length line))
		  (make-mark line (car newlines))))
	    (else
	     (and (substring-equal-ci? string (car newlines) end
				       (line-string line) 0
				       (line-length line))
		  (match-rest (line-next line)
			      (-1+ (car newlines))
			      (cdr newlines))))))

    (cond ((eq? start-line end-line)
	   (and (null? (cdr newlines))
		(let ((end-position* (- start-position (car newlines))))
		  (and (<= end-position end-position*)
		       (substring-equal-ci? string start end
					    (line-string start-line)
					    end-position* start-position)
		       (make-mark start-line end-position*)))))
	  ((null? (cdr newlines))
	   (let ((end-position* (- start-position (car newlines))))
	     (and (<= 0 end-position*)
		  (substring-equal-ci? string start end
				       (line-string start-line)
				       end-position* start-position)
		  (make-mark start-line end-position*))))
	  (else
	   (and (substring-equal-ci? string (car newlines) end
				     (line-string start-line) 0
				     start-position)
		(match-rest (line-next start-line)
			    (-1+ (car newlines))
			    (cdr newlines)))))))

;;;; Character Match

(define (match-next-char start end char)
  (and (mark< start end)
       (let ((line (mark-line start))
	     (position (mark-position start)))
	 (if (= position (line-length line))
	     (and (char=? char #\newline)
		  (make-mark (line-next line) 0))
	     (and (char=? char (string-ref (line-string line) position))
		  (make-mark line (1+ position)))))))

(define (match-previous-char start end char)
  (and (mark> start end)
       (let ((line (mark-line start))
	     (position (-1+ (mark-position start))))
	 (if (negative? position)
	     (and (char=? char #\newline)
		  (make-mark (line-previous line)
			     (line-length (line-previous line))))
	     (and (char=? char (string-ref (line-string line) position))
		  (make-mark line position))))))

(define (match-next-char-in-set start end char-set)
  (and (mark< start end)
       (char-set-member? char-set (mark-right-char start))
       (mark1+ start #!false)))

(define (match-previous-char-in-set start end char-set)
  (and (mark> start end)
       (char-set-member? char-set (mark-left-char start))
       (mark-1+ start #!false)))