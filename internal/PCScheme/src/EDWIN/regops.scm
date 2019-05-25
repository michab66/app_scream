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


;;;; Region Operations

;;;; String <-> Region

(define (string->region string)
  (substring->region string 0 (string-length string)))

(define (substring->region string start end)
  (let ((nl (substring-find-next-char string start end #\Newline)))
    (if (not nl)
        (let ((line (make-line (substring string start end))))
 	  (lines->region line line))
	(let ((first-line (make-line (substring string start nl)))
	      (group (make-group #!FALSE)))
	  (define (loop previous-line n start)
	    (let ((nl (substring-find-next-char string start end #\Newline)))
	      (if (not nl)
		  (let ((last-line (make-line (substring string start end))))
		    (connect-lines! previous-line last-line)
		    (set-line-group! last-line group)
		    (set-line-number! last-line n)
		    (let ((region
			   (components->region first-line 0 last-line
					       (line-length last-line))))
		      (%set-group-region! group region)
		      region))
		  (let ((this-line (make-line (substring string start nl))))
		    (connect-lines! previous-line this-line)
		    (set-line-group! this-line group)
		    (set-line-number! this-line n)
		    (loop this-line (+ n line-number-increment) (1+ nl))))))
	  (set-line-group! first-line group)
	  (set-line-number! first-line 0)
	  (loop first-line line-number-increment (1+ nl))))))

(define (region->string region)
  (region-components region
    (lambda (start-line start-position end-line end-position)
      (if (eq? start-line end-line)
	  (substring (line-string start-line) start-position end-position)
	  (let ((result (string-allocate (region-count-chars region))))
	    (define (loop target line)
	      (string-set! result target #\Newline)
	      (if (eq? line end-line)
		  (substring-move-right! (line-string end-line) 0 end-position
					 result (1+ target))
		  (begin (substring-move-right! (line-string line) 0
						(line-length line)
						result (1+ target))
			 (loop (+ target (line-length line) 1)
			       (line-next line)))))
	    (substring-move-right! (line-string start-line) start-position
				   (line-length start-line) result 0)
	    (loop (- (line-length start-line) start-position)
		  (line-next start-line))
	    result)))))

;;;; Copy Region

(define (region-copy region)
  (region-components region
    (lambda (start-line start-position end-line end-position)
      (if (eq? start-line end-line)
	  (let ((line (subline start-line start-position end-position)))
            (lines->region line line))
	  (let ((new-start (subline start-line
				    start-position
				    (line-length start-line)))
		(group (make-group #!FALSE)))
	    (define (loop this-line n new-previous)
	      (if (eq? this-line end-line)
		  (let ((new-end (subline end-line 0 end-position)))
		    (connect-lines! new-previous new-end)
		    (set-line-group! new-end group)
		    (set-line-number! new-end n)
		    (let ((region
			   (components->region new-start 0
					       new-end (line-length new-end))))
		      (%set-group-region! group region)
		      region))
		  (let ((new-this (line-copy this-line)))
		    (connect-lines! new-previous new-this)
		    (set-line-group! new-this group)
		    (set-line-number! new-this n)
		    (loop (line-next this-line)
			  (+ n line-number-increment)
			  new-this))))
	    (set-line-group! new-start group)
	    (set-line-number! new-start 0)
	    (loop (line-next start-line)
		  line-number-increment
		  new-start))))))

;;;; Extract Region

(define (region-extract! region)
  (let ((sync (region-delete-starting! region)))
    (let ((extracted-region (region-components region %region-extract!)))
      (sync extracted-region)
      extracted-region)))

(define %region-extract!
  (letrec
   ((%start-pos '())
    (%end-pos '())
    (%offset '())
    (%new-line '())
    (%receiver1
     (lambda (mark cursor?)
       (cond ((> (mark-position mark) %end-pos)
	      (set-mark-position! mark (- (mark-position mark) %offset)))
	     ((> (mark-position mark) %start-pos)
	      (set-mark-position! mark %start-pos)))))

    (%receiver2
     (lambda (mark cursor?)
       ((if cursor? %set-mark-line! set-mark-line!) mark %new-line)
       (set-mark-position! mark
			   (if (> (mark-position mark) %end-pos)
			       (- (mark-position mark) %offset)
			       %start-pos))))

    (%receiver3
     (lambda (mark cursor?)
       ((if cursor? %set-mark-line! set-mark-line!) mark %new-line)
       (set-mark-position! mark %start-pos)))

    (%receiver4
     (lambda (mark cursor?)
       ((if cursor? %set-mark-line! set-mark-line!) mark %new-line)
       (if (> (mark-position mark) %start-pos)
	   (set-mark-position! mark %start-pos)))))

   (lambda (start-line start-pos end-line end-pos)
     (letrec
      ((move-marks!
	(lambda (line)
	  (if (eq? line end-line)
	      (for-each-mark! end-line %receiver2)
	      (begin (for-each-mark! line %receiver3)
		     (move-marks! (line-next line)))))))
      (set! %start-pos start-pos)
      (set! %end-pos end-pos)
      (if (eq? start-line end-line)
	  (let ((offset (- end-pos start-pos)))
	    (set! %offset offset)
	    (for-each-mark! start-line %receiver1)
	    (let ((line (subline-extract! start-line start-pos end-pos)))
	      (lines->region line line)))
	  (let ((new-line (line-extract! start-line start-pos end-line end-pos))
		(offset (- end-pos start-pos))
		(start-previous (line-previous start-line))
		(end-next (line-next end-line)))
	    (set! %new-line new-line)
	    (set! %offset offset)
	    (for-each-mark! start-line %receiver4)
	    (move-marks! (line-next start-line))
	    (set-line-group! new-line (line-group start-line))
            (set! %new-line '())
	    (disconnect-lines! start-line end-line)
	    (connect-lines! start-previous new-line)
	    (connect-lines! new-line end-next)
	    (number-lines! new-line new-line)
	    (lines->region start-line end-line)))))))

;;;; Insert Region

(define (region-insert! mark region)
  (let ((sync (region-insert-starting! mark)))
    (let ((region*
            (region-components region
              (lambda (start-line start-pos end-line end-pos)
                ((lambda (line pos)
		   (%region-insert! line pos
				    start-line start-pos
				    end-line end-pos))
                 (mark-line mark) (mark-position mark) )))))
      (sync region*)
       region*)))

(define %region-insert!
  (letrec
    ((%pos '())
     (%offset '())
     (%end-line '())
     (%end-pos '())
     (%receiver1
      (lambda (mark cursor?)
	(if (or (> (mark-position mark) %pos)
		(and (= (mark-position mark) %pos)
		     (mark-left-inserting? mark)))
	    (set-mark-position! mark (+ (mark-position mark) %offset)))))

     (%receiver2
      (lambda (mark cursor?)
	(cond ((> (mark-position mark) %pos)
	       ((if cursor? %set-mark-line! set-mark-line!) mark %end-line)
	       (set-mark-position! mark (+ (mark-position mark) %offset)))
	      ((and (= (mark-position mark) %pos)
		    (mark-left-inserting? mark))
	       ((if cursor? %set-mark-line! set-mark-line!) mark %end-line)
	       (set-mark-position! mark %end-pos))))))

  (lambda (line pos start-line start-pos end-line end-pos)
  (set! %pos pos)
  (if (eq? start-line end-line)
      (let ((offset (- end-pos start-pos)))
        (set! %offset offset)
	(for-each-mark! line %receiver1)
	(line-insert! line pos start-line start-pos end-pos)
	(%make-region (%make-mark line pos #!FALSE)
		      (%make-mark line (+ pos offset) #!TRUE)))
      (let ((offset (- end-pos pos)))
        (set! %end-line end-line)
        (set! %offset offset)
        (set! %end-pos end-pos)
	(for-each-mark! line %receiver2)
	(line-splice! line pos start-line start-pos end-line end-pos)
        (set! %end-line '())
	(connect-lines! end-line (line-next line))
	(connect-lines! line (line-next start-line))
	(number-lines! (line-next line) end-line)
	(%make-region (%make-mark line pos #!FALSE)
		      (%make-mark end-line end-pos #!TRUE)))))))

;;; These are overwritten by the routines in insertch.scm
;;;(define (region-insert-char! mark char)
;;;  (if (char= char #\Newline)
;;;      (region-insert-newline! mark)
;;;      (let ((sync (region-insert-starting! mark)))
;;;        (let ((region (mark-components mark
;;;                        (lambda (line pos)
;;;                          (%region-insert-char! line pos char)))))
;;;          (sync region)
;;;          region))))
;;;
;;;(define (%region-insert-char! line pos char)
;;;  (for-each-mark! line
;;;    (lambda (mark)
;;;      (if (or (> (mark-position mark) pos)
;;;	      (and (= (mark-position mark) pos)
;;;		   (mark-left-inserting? mark)))
;;;	  (set-mark-position! mark (1+ (mark-position mark))))))
;;;  (line-insert-char! line pos char)
;;;  (%make-region (%make-mark line pos #!FALSE)
;;;		(%make-mark line (1+ pos) #!TRUE)))
;;;
(define (region-insert-newline! mark)
  (let ((sync (region-insert-starting! mark)))
    (let ((region
	    ((lambda (line pos)
		(%region-insert-newline! line pos))
             (mark-line mark) (mark-position mark))))
      (sync region)
      region)))

(define %region-insert-newline!
  (letrec
    ((%pos '())
     (%new-next '())
     (%receiver
      (lambda (mark cursor?)
	(cond ((> (mark-position mark) %pos)
	       ((if cursor? %set-mark-line! set-mark-line!) mark %new-next)
	       (set-mark-position! mark (- (mark-position mark) %pos)))
	      ((and (= (mark-position mark) %pos)
		    (mark-left-inserting? mark))
	       ((if cursor? %set-mark-line! set-mark-line!) mark %new-next)
	       (set-mark-position! mark 0))))))

  (lambda (line pos)
  (let ((new-next (subline-extract! line pos (line-length line))))
    (set! %pos pos)
    (set! %new-next new-next)
    (for-each-mark! line %receiver)
    (set! %new-next '())
    (connect-lines! new-next (line-next line))
    (connect-lines! line new-next)
    (number-lines! new-next new-next)
    (%make-region (%make-mark line (line-length line) #!FALSE)
		  (%make-mark new-next 0 #!TRUE))))))

;;; This should be implemented later for speed.
(define region-delete!
  region-extract!)

(define (region-insert mark region)
  (region-insert! mark (region-copy region)))

(define (region-insert-string! mark string)
  (region-insert! mark (string->region string)))


;;;; Line String Operations

(define (subline line start end)
  (make-line (substring (line-string line) start end)))

(define (line-copy line)
  (make-line (line-string line)))

(define (subline-extract! line start end)
  (let ((new-line (subline line start end)))
    (set-line-string! line (string-delete (line-string line) start end))
    new-line))

(define (line-extract! start-line start-pos end-line end-pos)
  (let ((start-string (line-string start-line))
	(end-string (line-string end-line)))
    (let ((AD (substring-append start-string 0 start-pos
				end-string end-pos (string-length end-string)))
	  (B (substring start-string start-pos (string-length start-string)))
	  (C (substring end-string 0 end-pos)))
      (set-line-string! start-line B)
      (set-line-string! end-line C)
      (make-line AD))))

(define (line-insert! line1 start1 line2 start2 end2)
  (set-line-string!
   line1
   (string-insert-substring (line-string line1) start1
			    (line-string line2) start2 end2)))

(define (line-insert-char! line start char)
  (set-line-string!
   line
   (let ((string (line-string line)))
     (%string-append string 0 start
                     char
                     string start (string-length string)))))

(define (line-splice! line1 position1 line2 position2 line3 position3)
  (let ((string1 (line-string line1))
	(string2 (line-string line2))
	(string3 (line-string line3)))
    (set-line-string! line1
		      (substring-append string1 0 position1
					string2
					position2
					(string-length string2)))
    (set-line-string! line3
		      (substring-append string3 0 position3
					string1
					position1
					(string-length string1)))))

(define (mark-left-char mark)
  (cond ((group-start? mark)
	 (error "No left character" mark))
	((line-start? mark)
	 #\Newline)
	(else
	 (string-ref (line-string (mark-line mark))
		     (-1+ (mark-position mark))))))

(define (mark-right-char mark)
  (cond ((group-end? mark)
	 (error "No right character" mark))
	((line-end? mark)
	 #\Newline)
	(else
	 (string-ref (line-string (mark-line mark))
		     (mark-position mark)))))