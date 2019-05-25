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

;;; 4:01pm  Tuesday, 25 June 1985
(begin
(define-integrable char-equal-ci? char-ci=?)

(define-integrable char-equal? char=?)

(define-integrable char-less-ci? char-ci<?)

(define-integrable char-less? char<?)

(define-integrable string-equal-ci? string-ci=?)

(define-integrable string-equal? string=?)

(define-integrable string-less-ci? string-ci<?)

(define-integrable string-less? string<?)

(define-integrable substring-equal-ci? substring-ci=?)

(define-integrable substring-equal? substring=?)

(define-integrable substring-less-ci? substring-ci<?)

(define-integrable  substring-less? substring<?)

(define-integrable char= char=?)

(define-integrable char< char<?)

)
(define char-upper-case?
   (lambda (c)
     (char=? (char-upcase c) c)))

(define char-lower-case?
   (lambda (c)
     (char=? (char-downcase c) c)))

(macro string-allocate
   (lambda (e)
     (list 'make-string (cadr e) nil)))



;;;; Comparison Primitives

(define substring-match-forward)
(define substring-match-forward-ci)

(let ()
  (define (make-substring-match-forward char-equal?)
    (lambda (string1 start1 end1 string2 start2 end2)
      (define (loop index1 index2 n)
	(if (or (= index1 end1)
		(= index2 end2)
		(not (char-equal? (string-ref string1 index1)
				  (string-ref string2 index2))))
	    n
	    (loop (1+ index2) (1+ index2) (1+ n))))
      (loop start1 start2 0)))
  (set! substring-match-forward
	(make-substring-match-forward char-equal?))
  (set! substring-match-forward-ci
	(make-substring-match-forward char-equal-ci?)))

(define string-match-forward)
(define string-match-forward-ci)

(let ()
  (define (string-comparison substring-comparison)
    (lambda (string1 string2)
      (substring-comparison string1 0 (string-length string1)
			    string2 0 (string-length string2))))
  (set! string-match-forward
	(string-comparison substring-match-forward))
  (set! string-match-forward-ci
	(string-comparison substring-match-forward-ci)))

;;;; Character Search Primitives

(define substring-find-next-char-ci
  (lambda (string start end char)
    (let ((char1 (char-upcase char))
          (char2 (char-downcase char)))
      (let ((set (make-string 2 char1)))
        (substring-find-next-char-in-set string start end 
                                         (string-set! set 1 char2))))))

(define substring-find-previous-char-ci
  (lambda (string start end char)
    (let ((char1 (char-upcase char))
          (char2 (char-downcase char)))
      (let ((set (make-string 2 char1)))
        (substring-find-previous-char-in-set string start end 
                                         (string-set! set 1 char2))))))

