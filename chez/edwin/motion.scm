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

;;;; Motion within Groups

;;;; Mark Movement

(begin
(define-integrable group-start
  (lambda (mark)
    (%group-start (mark-group mark))))

(define-integrable group-end
  (lambda (mark)
    (%group-end (mark-group mark))))
)
(define (group-start? mark)
  (mark= (group-start mark) mark))

(define (group-end? mark)
  (mark= (group-end mark) mark))

(define (line-start? mark)
  (or (group-start? mark)
      (zero? (mark-position mark))))

(define (line-end? mark)
  (or (group-end? mark)
      (= (mark-position mark) (line-length (mark-line mark)))))

(define (limit-mark-motion limit? limit)
  (cond ((eq? limit? 'LIMIT) limit)
	((eq? limit? 'BEEP) (beep) limit)
	((eq? limit? 'ERROR) (editor-error))
	((not limit?) #!FALSE)
	(else (error "Unknown limit type" limit?))))

(define (mark1+ mark limit?)
  (cond ((group-end? mark)
	 (limit-mark-motion limit? mark))
	((= (mark-position mark)
	    (line-length (mark-line mark)))
	 (make-mark (line-next (mark-line mark))
		    0))
	(else
	 (make-mark (mark-line mark)
		    (1+ (mark-position mark))))))

(define (mark-1+ mark limit?)
  (cond ((group-start? mark)
	 (limit-mark-motion limit? mark))
	((zero? (mark-position mark))
	 (make-mark (line-previous (mark-line mark))
		    (line-length (line-previous (mark-line mark)))))
	(else
	 (make-mark (mark-line mark)
		    (-1+ (mark-position mark))))))

(define (mark+ mark n limit?)
  (cond ((positive? n)
	 (let ((end-mark (group-end mark)))
	   (let ((end-line (mark-line end-mark))
		 (end-position (mark-position end-mark)))
	     (define (loop line position n)
	       (if (eq? line end-line)
		   (let ((new-position (+ position n)))
		     (if (<= new-position end-position)
			 (make-mark line new-position)
			 (limit-mark-motion limit? end-mark)))
		   (let ((room (- (line-length line) position)))
		     (if (<= n room)
			 (make-mark line (+ position n))
			 (loop (line-next line) 0 (- n (1+ room)))))))
	     (loop (mark-line mark) (mark-position mark) n))))
	((negative? n) (mark- mark (- n) limit?))
	(else mark)))

(define (mark- mark n limit?)
  (cond ((positive? n)
	 (let ((start-mark (group-start mark)))
	   (let ((start-line (mark-line start-mark))
		 (start-position (mark-position start-mark)))
	     (define (loop line position n)
	       (cond ((eq? line start-line)
		      (let ((new-position (- position n)))
			(if (<= start-position new-position)
			    (make-mark line new-position)
			    (limit-mark-motion limit? start-mark))))
		     ((<= n position)
		      (make-mark line (- position n)))
		     (else
		      (loop (line-previous line)
			    (line-length (line-previous line))
			    (- n (1+ position))))))
	     (loop (mark-line mark) (mark-position mark) n))))
	((negative? n) (mark+ mark (- n) limit?))
	(else mark)))

(define (region-count-chars region)
  (region-components region
    (lambda (start-line start-position end-line end-position)
      (define (loop line accumulator)
	(if (eq? line end-line)
	    (+ end-position accumulator)
	    (loop (line-next line)
		  (1+ (+ (line-length line) accumulator)))))
      (if (eq? start-line end-line)
	  (- end-position start-position)
	  (loop (line-next start-line)
		(1+ (- (line-length start-line) start-position)))))))

;;;; Mark Comparison

(define (mark= mark1 mark2)
  (and (eq? (mark-line mark1) (mark-line mark2))
       (= (mark-position mark1) (mark-position mark2))))

(define (mark< mark1 mark2)
  (if (eq? (mark-line mark1) (mark-line mark2))
      (< (mark-position mark1) (mark-position mark2))
      (and (eq? (line-group (mark-line mark1))
		(line-group (mark-line mark2)))
	   (< (line-number (mark-line mark1))
	      (line-number (mark-line mark2))))))

(define (mark<= mark1 mark2)
  (if (eq? (mark-line mark1) (mark-line mark2))
      (<= (mark-position mark1) (mark-position mark2))
      (and (eq? (line-group (mark-line mark1))
		(line-group (mark-line mark2)))
	   (< (line-number (mark-line mark1))
	      (line-number (mark-line mark2))))))

(define (mark> mark1 mark2)
  (if (eq? (mark-line mark1) (mark-line mark2))
      (> (mark-position mark1) (mark-position mark2))
      (and (eq? (line-group (mark-line mark1))
		(line-group (mark-line mark2)))
	   (> (line-number (mark-line mark1))
	      (line-number (mark-line mark2))))))


;;;; Line Movement

(define (line-offset line n if-ok if-not-ok)
  (cond ((negative? n)
	 (let ((limit (mark-line (%group-start (line-group line)))))
	   (define (loop- line n)
	     (cond ((zero? n) (if-ok line))
		   ((eq? line limit) (if-not-ok limit))
		   (else (loop- (line-previous line) (1+ n)))))
	   (if (eq? line limit)
	       (if-not-ok limit)
	       (loop- (line-previous line) (1+ n)))))
	(else
	 (let ((limit (mark-line (%group-end (line-group line)))))
	   (define (loop+ line n)
	     (cond ((zero? n) (if-ok line))
		   ((eq? line limit) (if-not-ok limit))
		   (else (loop+ (line-next line) (-1+ n)))))
	   (loop+ line n)))))

(define (line-start mark n limit?)
  (line-offset (mark-line mark) n
	       (lambda (line)
		 (if (eq? line (mark-line (group-start mark)))
		     (group-start mark)
		     (make-mark line 0)))
	       (lambda (line)
		 (limit-mark-motion limit?
				    (if (negative? n)
					(group-start mark)
					(group-end mark))))))

(define (line-end mark n limit?)
  (line-offset (mark-line mark) n
	       (lambda (line)
		 (if (eq? line (mark-line (group-end mark)))
		     (group-end mark)
		     (make-mark line (line-length line))))
	       (lambda (line)
		 (limit-mark-motion limit?
				    (if (negative? n)
					(group-start mark)
					(group-end mark))))))