;;;; Permanent Marks

;;; The marks list is cleaned every time that a mark is added to the list,
;;; and every time that FOR-EACH-MARK! is called.  This should keep the
;;; number of extraneous entries to a minimum.  Note that FOR-EACH-MARK!
;;; and SET-MARK-LINE! are intended to be used together; in particular,
;;; a great deal of cleverness has been used to ensure that the changes
;;; made by SET-MARK-LINE! are noticed by FOR-EACH-MARK!.  This turned out
;;; to be non-trivial to implement.

(define (mark-permanent! mark)
  (let ((n (object-hash mark))
	(marks (line-marks (mark-line mark))))
    (if (not (memv n marks))
	(let ((marks (cons n marks)))
	  (begin (clean-marks-tail! marks)
		 (set-line-marks! (mark-line mark) marks)))))
  mark)

(define (clean-marks-tail! marks)
  (if (not (null? (cdr marks)))
      (if (object-unhash (cadr marks))
	  (clean-marks-tail! (cdr marks))
	  (begin (set-cdr! marks (cddr marks))
		 (clean-marks-tail! marks)))))

(define (for-each-mark! line procedure)
  (define (loop-1 marks)
    (if (not (null? marks))
	(let ((mark (object-unhash (car marks))))
	  (if mark
	      (begin (procedure mark #!false)
		     (if (eq? marks (line-marks line))
			 (loop-2 marks (cdr marks))
			 (loop-1 (line-marks line))))
	      (begin (set-line-marks! line (cdr marks))
		     (loop-1 (line-marks line)))))))
  (define (loop-2 previous marks)
    (if (not (null? marks))
	(let ((mark (object-unhash (car marks))))
	  (if mark
	      (begin (procedure mark #!false)
		     (if (eq? marks (cdr previous))
			 (loop-2 marks (cdr marks))
			 (loop-2 previous (cdr previous))))
	      (begin (set-cdr! previous (cddr previous))
		     (loop-2 previous (cdr previous)))))))

;;; point is treated as a special case and is no longer a permanent mark
;;; This would decrease the number of permanent marks considerably.
;;; Permannet marks are not so cheap and should be used only when
;;; really needed. Currently the point is obtained from current point
;;; but in a general setting there should be a way to get back to the
;;; buffer from group to get  the point.

  (let ((point (current-point)))
    (if (and (eq? line (mark-line point))
             (let ((n (object-hash point)))
               (not (memv n (line-marks line)))))
        (procedure point #!true)))
  (loop-1 (line-marks line)))

(define (set-mark-line! mark new-line)
  (let ((old-line (mark-line mark)))
    (cond ((not (eq? old-line new-line))
	   (let ((marks
		  (let ((n (object-hash mark))
			(marks (line-marks old-line)))
		    (define (loop previous marks)
		      (if (= n (car marks))
			  (begin (set-cdr! previous (cdr marks))
				 marks)
			  (loop marks (cdr marks))))
		    (if (= n (car marks))
			(begin (set-line-marks! old-line (cdr marks))
			       marks)
			(loop marks (cdr marks))))))
	     (%set-mark-line! mark new-line)
	     (set-cdr! marks (line-marks new-line))
	     (clean-marks-tail! marks)
	     (set-line-marks! new-line marks))))))


