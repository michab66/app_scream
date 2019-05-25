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


(define char<=
   (lambda (a1 c e1)
      (and (char<=? a1 c)
           (char<=? c e1))))

(define (char-set-member? char-set char)
  (substring-find-next-char-in-set char-set 0 (string-length char-set) char))

;;; Character Sets

(define char-set:whitespace
  (make-char-set #\Space #\Newline
	    #\Tab #\Return #\Page
	    ))

(define char-set:not-whitespace
  (char-set-invert char-set:whitespace))

(define char-set-predicate
  (lambda (char-set)
    (let ((len (string-length char-set)))
      (lambda (char)
        (substring-find-next-char-in-set char-set 0 len char)))))

(define char-whitespace? (char-set-predicate char-set:whitespace))

(define char-set:alphabetic
  (predicate->char-set
    (lambda (char)
      (or (char<= #\A char #\Z)
          (char<= #\a char #\z)))))

(define char-alphabetic? (char-set-predicate char-set:alphabetic))

(define char-set:alphanumeric
  (predicate->char-set
    (lambda (char)
      (or (char<= #\0 char #\9)
          (char<= #\A char #\Z)
          (char<= #\a char #\z)))))

(define char-set:graphic
  (char-set-union char-set:alphanumeric
		  (make-char-set #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\*
			    #\+ #\, #\- #\. #\/ #\: #\; #\< #\= #\>
			    #\? #\@ #\[ #\\ #\] #\^ #\_ #\` #\{ #\|
			    #\} #\~ #\Space)))

(define char-graphic? (char-set-predicate char-set:graphic))

(define sexp-delims (make-char-set #\( #\)))

(define char-set:blanks (make-char-set #\Space #\Tab))
(define char-blank? (char-set-predicate char-set:blanks))
(define char-set:non-blanks (char-set-invert char-set:blanks))
;define find-next-blank (char-set-forward-search char-set:blanks))
;define find-previous-blank (char-set-backward-search char-set:blanks))

(define word-constituent-chars
  (char-set-union char-set:alphanumeric
		  (make-char-set #\$ #\% #\.)))

(define word-delimiter-chars
  (char-set-invert word-constituent-chars))

(define sexp-constituent-chars
  (char-set-union char-set:alphanumeric
        (make-char-set #\! #\$ #\% #\* #\/ #\: #\< #\= #\> #\? #\_
                       #\- #\+ #\~ #\@ #\# #\^)))

(define sexp-delimeter-chars (char-set-invert sexp-constituent-chars))

(define char-set-sexp? (char-set-predicate sexp-constituent-chars))


(define non-graphic-chars (make-non-graphic-char-set))



