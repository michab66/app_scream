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

;;; moving forward

(define (forward-one-list start end)
  (forward-sexp:top start end 0))

(define (forward-down-one-list start end)
  (forward-sexp:top start end -1))

(define (forward-up-one-list start end)
  (forward-sexp:top start end 1))

(define forward-sexp:top
  (lambda (start end depth)
    (letrec
      ((forward-sexp:top
         (lambda (start end depth)
	   (and (mark< start end)
		(search-forward start end depth))))

       (search-forward
	(lambda (start end depth)
	  (let ((mark (find-next-char-in-set start end sexp-delims)))
	    (and mark
		 (cond
		  ((char=? (mark-right-char mark)   ;;; (
			   #\) )
                   (list-forward-close (mark1+ mark #!false) end depth))
                  (else (list-forward-open (mark1+ mark #!false)
                                           end depth)))))))

      (list-forward-open
        (lambda (start end depth)
	  (if (= depth -1)
	      start
	      (forward-sexp:top start end (1+ depth)))))

      (list-forward-close
        (lambda (start end depth)
	  (and (> depth 0)
	       (if (= depth 1)
		   start
		   (forward-sexp:top start end (-1+ depth)))))))
    (forward-sexp:top start end depth))))


;;; sexp movement

(define (forward-one-sexp start end )
    (let ((m (find-next-char-in-set start end char-set:not-whitespace)))
      (if m
          (let ((char (mark-right-char m)))
            (cond ((char=? char #\( )         ;;; )
                   (forward-one-list m end))
                  ((char-set-sexp? char)
                   (find-next-char-in-set m end sexp-delimeter-chars))
                  ((char=? char #\")      ;;;"
                   (find-next-closing-quote (mark1+ m #!false) end)) ;;;)
                  ((char=? char #\)) (mark1+ m #!false))   ;;; (
                  ((or (char=? char #\') (char=? char #\`))
                   (forward-one-sexp (mark1+ m #!false) end))
                  (else (find-next-char-in-set m end char-set:whitespace))))
          #!false)))

(define (backward-one-sexp start end )
    (let ((m (find-previous-char-in-set start end char-set:not-whitespace)))
      (if m
          (let ((char (mark-left-char m)))
            (cond ((char=? char #\) )         ;;; (
                   (backward-one-list m end))
                  ((char-set-sexp? char)
                   (find-previous-char-in-set m end sexp-delimeter-chars))
                  ((char=? char #\")      ;;;"
                   (find-previous-closing-quote (mark-1+ m #!false) end)) ;;;)
                  ((char=? char #\()     ;;;)
                   (mark-1+ m #!false))
                  ((or (char=? char #\') (char=? char #\`))
                   (backward-one-sexp (mark-1+ m #!false) end))
                  (else (find-previous-char-in-set m end
                                                   char-set:whitespace))))
          #!false)))

(define find-next-closing-quote
  (lambda (start end)
    (let ((m (find-next-char-in-set start end string-quote)))
      (and m
          (mark1+ m #!false)))))

(define find-previous-closing-quote
  (lambda (start end)
    (let ((m (find-previous-char-in-set start end string-quote)))
      (and m
          (mark-1+ m #!false)))))

(define string-quote (make-string 1 #\"))


;;; moving backward

(define (backward-down-one-list start end)
  (backward-sexp:top start end -1))

(define (backward-up-one-list start end)
  (backward-sexp:top start end 1))

(define forward-list)
(define backward-list)
(make-motion-pair forward-one-list backward-one-list
  (lambda (f b)
    (set! forward-list f)
    (set! backward-list b)))

(define forward-down-list)
(define backward-down-list)
(make-motion-pair forward-down-one-list backward-down-one-list
  (lambda (f b)
    (set! forward-down-list f)
    (set! backward-down-list b)))

(define forward-up-list)
(define backward-up-list)
(make-motion-pair forward-up-one-list backward-up-one-list
  (lambda (f b)
    (set! forward-up-list f)
    (set! backward-up-list b)))

;;;

(define forward-sexp '())
(define backward-sexp '())

(make-motion-pair forward-one-sexp backward-one-sexp
  (lambda (f b)
    (set! forward-sexp f)
    (set! backward-sexp b)))



;;; Lisp Indenting

(define scheme:delim (char-set-union char-set:whitespace sexp-delims))

(define lisp-indent-line
  (lambda (point)
    (letrec
      ((calculate-lisp-indent
         (lambda (mark)
	   (let ((containing-sexp
		   (backward-up-one-list mark (group-start mark))))
	     (if containing-sexp
		 (let ((next-sexp-start
			(find-next-char-in-set
                              (mark1+ containing-sexp #!false) mark
					       char-set:not-whitespace)))
		   (if next-sexp-start
		       (if (char-ci=? #\( (mark-right-char next-sexp-start));)
			   (mark-column next-sexp-start)
			   (let ((next-sexp-end
				  (find-next-char-in-set next-sexp-start mark
							 scheme:delim)))
			     (table-lookup containing-sexp next-sexp-start
					   next-sexp-end mark)))
		       (1+ (mark-column containing-sexp))))
		 0))))

       (table-lookup
         (lambda (containing-sexp sexp-start sexp-end limit-mark)
	   (let ((string (substring (line-string (mark-line sexp-start))
				    (mark-position sexp-start)
				    (mark-position sexp-end))))
	     (cond ((is-string-member? string %standard-funcs)
		    (+ lisp-indent (mark-column containing-sexp)))
		   (else (let ((m (find-next-char-in-set sexp-end limit-mark
						 char-set:not-whitespace)))
			   (if (and m
                                    (not (char=? (mark-right-char m) #\;)))
			       (mark-column m)
			       (+ lisp-indent
				  (mark-column containing-sexp)))))))))

	 (is-string-member?
            (lambda (string list1)
              (if list1
                  (if (string-ci=? string (car list1))
                      #!true
                      (is-string-member? string (cdr list1)))
                  #!false))))

       (let* ((start-mark (line-start point 0 #!false))
	      (start (horizontal-space-end (line-start point 0 #!false))))
	 (let ((indentation (calculate-lisp-indent start)))
           (if (<> indentation (mark-column start))
               (begin
		(region-delete! (make-region start-mark start))
		(insert-chars #\space indentation start-mark))))))))

(define %standard-funcs
  '("define" "lambda" "let" "letrec" "let*" "fluid-let" "macro" "rec" "named-lambda" "call/cc" "case" "with-input-from-file" "call-with-input-file"))




(define lisp-indent-sexp
  (lambda (point)
    (letrec
      ((end (line-start (forward-sexp point 1 'ERROR) 0 #!false))
       (loop
	(lambda (start)
	  (lisp-indent-line start)
	  (if (not (mark= start end))
	      (loop (line-start start 1 #!false))))))
      (if (mark< point end)
	  (loop (line-start point 1 #!false))))))





