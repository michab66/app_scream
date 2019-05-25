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

(begin
(define-integrable %make-region
  (lambda (start end)
    (cons start end)))

(define-integrable region-start
  (lambda (region)
    (car region)))

(define-integrable region-end
  (lambda (region)
    (cdr region)))

(define-integrable region-group
  (lambda (region)
    (mark-group (region-start region))))

(define-integrable components->region
  (lambda (start-line start-pos end-line end-pos)
    (%make-region (mark-permanent! (%make-mark start-line start-pos #!FALSE))
	          (mark-permanent! (%make-mark end-line end-pos #!TRUE)))))

(define-integrable make-mark
  (lambda (line position)
    (%make-mark line position #!TRUE)))

(define-integrable %make-mark
  (lambda (line position left-inserting?)
    (let ((mark (make-vector 3)))
      (vector-set! mark 0 line)
      (vector-set! mark 1 position)
      (vector-set! mark 2 left-inserting?)
      mark)))

(define-integrable mark-line
  (lambda (mark)
    (vector-ref mark 0)))

(define-integrable %set-mark-line!
  (lambda (mark line)
    (vector-set! mark 0 line)))

(define-integrable mark-position
  (lambda (mark)
    (vector-ref mark 1)))

(define-integrable set-mark-position!
  (lambda (mark position)
    (vector-set! mark 1 position)))

(define-integrable mark-left-inserting?
  (lambda (mark)
    (vector-ref mark 2)))

(define-integrable mark-group
  (lambda (mark)
    (line-group (mark-line mark))))

(define-integrable line-tag 'line)

(define-integrable make-line
  (lambda (string)
    (let ((line (make-vector 8)))
      (vector-set! line 3 line-tag)
      (vector-set! line 1 string)
      line)))

(define-integrable line-string
  (lambda (line)
    (vector-ref line 1)))

(define-integrable line-previous
  (lambda (line)
    (vector-ref line 2)))

(define-integrable line-next
  (lambda (line)
    (vector-ref line 0)))

(define-integrable line-marks
  (lambda (line)
    (vector-ref line 4)))

(define-integrable set-line-marks!
  (lambda (line marks)
    (vector-set! line 4 marks)))

(define-integrable line-group
  (lambda (line)
    (vector-ref line 5)))

(define-integrable set-line-group!
  (lambda (line group)
    (vector-set! line 5 group)))

(define-integrable line-number
  (lambda (line)
    (vector-ref line 6)))

(define-integrable set-line-number!
  (lambda (line number)
    (vector-set! line 6 number)))

(define-integrable line-alist
  (lambda (line)
    (vector-ref line 7)))

(define-integrable set-line-alist!
  (lambda (line alist)
  (vector-set! line 7 alist)))
)
;;;; Text Data Structures

;;; This file describes the data structures used to represent and
;;; manipulate text within the editor.

;;; The basic unit of text is the GROUP, which is essentially a type
;;; of character string with some special operations.  Normally a
;;; group is modified by side effect; unlike character strings, groups
;;; will grow and shrink appropriately under such operations.  Also,
;;; it is possible to have pointers into a group, called MARKs, which
;;; continue to point to the "same place" under these operations; this
;;; would not be true of a string, elements of which are pointed at by
;;; indices.

;;; As is stressed in the EMACS manual, marks point between characters
;;; rather than directly at them.  This perhaps counter-intuitive
;;; concept may aid understanding.

;;; Besides acting as pointers into a group, marks may be compared.
;;; All of the marks within a group are totally ordered, and the
;;; standard order predicates are supplied for them.  In addition,
;;; marks in different groups are unordered with respect to one
;;; another.  The standard predicates have been extended to be false
;;; in this case, and another predicate, which indicates whether they
;;; are related, is supplied.

;;; Marks may be paired into units called REGIONs.  Each region has a
;;; START mark and an END mark, and it must be the case that START is
;;; less than or equal to END in the mark ordering.  While in one
;;; sense this pairing of marks is trivial, it can also be used to
;;; reduce overhead in the implementation since a region guarantees
;;; that its marks satisfy this very basic relation.

;;; As in most other editors of this type, there is a distinction
;;; between "temporary" and "permanent" marks.  The purpose for this
;;; distinction is that temporary marks require less overhead to
;;; create.  Conversely, temporary marks do not remain valid when
;;; their group is modified.  They are intended for local use when it
;;; is known that the group will remain unchanged.

;;; The implementation of marks is different from previous
;;; implementations.  In particular, it is not possible to tell
;;; whether a mark is temporary or permanent.  Instead, a "caller
;;; saves"-like convention is used.  Whenever any given mark needs to
;;; be permanent, one merely calls a procedure which "permanentizes"
;;; it.  All marks are created temporary by default.

;;; Internally, groups are represented as an ordered set of objects,
;;; called LINEs, which are doubly linked to form a linear chain.
;;; Each line represents a string of characters without newlines, and
;;; two adjacent lines are separated by a "virtual newline".  Thus
;;; this data structure directly corresponds to our intuitive concept
;;; of "line".

;;; In some sense the choice of lines are the unit of text is quite
;;; arbitrary; there are no real technical benefits to be gained from
;;; the choice.  The decision to structure things this way was based
;;; on the fact that most current editors are built that way, and
;;; expediency demands that we not innovate too much.

;;; With that said, it is important to restate that lines are an
;;; INTERNAL data representation.  Since the choice is arbitrary, they
;;; are not supported by any public operations.

;;;; Groups

;;; Every line belongs to a unique group, and every line belonging to
;;; the same group is related.  That is, the lines in a group are
;;; totally ordered.  Lines in different groups have no relation.

;;; There is no sharing of lines between groups.  When lines are
;;; copied out of a group, they form a new group.  When they are
;;; inserted into a group, they become part of that group.

(define make-group)
(let ()

(define group-tag 'group)

(set! make-group
(named-lambda (make-group region)
  (let ((group (make-vector 6)))
    (vector-set! group 2 group-tag)
    (vector-set! group 1 region)
    (vector-set! group 0 region)
    (vector-set! group 5 #!FALSE)
    group)))

)
(begin
(define-integrable group-index:total-region 1)
(define-integrable group-index:region 0)
(define-integrable group-index:delete-daemons 3)
(define-integrable group-index:insert-daemons 4)
(define-integrable group-index:read-only-flag 5)

(define-integrable group-region
  (lambda (group)
    (vector-ref group group-index:region)))

(define (%set-group-region! group region)
  (vector-set! group group-index:total-region region)
  (vector-set! group group-index:region region))

(define-integrable %group-start
  (lambda (group)
    (region-start (group-region group))))

(define-integrable %group-end
  (lambda (group)
    (region-end (group-region group))))
)

(define (group-read-only? group)
  (vector-ref group group-index:read-only-flag))

(define (set-group-read-only! group)
  (vector-set! group group-index:read-only-flag #!TRUE))

(define (set-group-writeable! group)
  (vector-set! group group-index:read-only-flag #!FALSE))


;;;; Group Modification Daemons

(define (group-delete-daemons group)
  (vector-ref group group-index:delete-daemons))

(define (add-group-delete-daemon! group daemon)
  (vector-set! group group-index:delete-daemons
	       (cons daemon (vector-ref group group-index:delete-daemons))))

(define (region-delete-starting! region)
  (if (group-read-only? (region-group region))
      (editor-error "Trying to modify read only text."))
  (region-modification-starting! (group-delete-daemons (region-group region))
				 region))

(define (group-insert-daemons group)
  (vector-ref group group-index:insert-daemons))

(define (add-group-insert-daemon! group daemon)
  (vector-set! group group-index:insert-daemons
	       (cons daemon (vector-ref group group-index:insert-daemons))))

(define (region-insert-starting! mark)
  (if (group-read-only? (mark-group mark))
      (editor-error "Trying to modified read only text."))
  (region-modification-starting! (group-insert-daemons (mark-group mark))
				 mark))

(define (region-modification-starting! all-daemons argument)
  (define (loop daemons)
    (if (null? daemons)
	'()
	(let ((sync ((car daemons) argument)))
	  (if sync
	      (cons sync (loop (cdr daemons)))
	      (loop (cdr daemons))))))
  (sync-daemons (loop all-daemons)))

(define ((sync-daemons daemons) region)
  (define (loop daemons)
    (if (not (null? daemons))
	(begin ((car daemons) region)
	       (loop (cdr daemons)))))
  (loop daemons))

;;;; Regions

(define (make-region start end)
  (cond ((mark<= start end) (%make-region start end))
	((mark<= end start) (%make-region end start))
	(else (error "Marks not related" start end))))

(define (lines->region start-line end-line)
  (let ((region (components->region start-line 0
				    end-line (line-length end-line))))
    (set-line-group! start-line (make-group region))
    (number-lines! start-line end-line)
    region))

(define (region-components region receiver)
  (receiver (mark-line (region-start region))
	    (mark-position (region-start region))
	    (mark-line (region-end region))
	    (mark-position (region-end region))))

;;;; Marks

(define (mark-components mark receiver)
  (receiver (mark-line mark)
	    (mark-position mark)))

(define (mark-right-inserting mark)
  (mark-permanent!
   (if (mark-left-inserting? mark)
       (%make-mark (mark-line mark) (mark-position mark) #!FALSE)
       mark)))

(define (mark-left-inserting mark)
  (mark-permanent!
   (if (mark-left-inserting? mark)
       mark
       (%make-mark (mark-line mark) (mark-position mark) #!TRUE))))


;;;; Lines

;;; Instead of using VECTOR, MAKE-LINE is coded in a strange way to
;;; make it maximally fast.  Both LIST->VECTOR and CONS are
;;; primitives.  Also, VECTOR would cons a list, then vectorize it,
;;; creating a bunch of garbage, while this only makes one cons.

(define (set-line-string! line string)
  (vector-set! line 1 string)
  (set-line-alist! line '()))

(define (connect-lines! previous next)
  (if (not (null? previous)) (vector-set! previous 0 next))
  (if (not (null? next)) (vector-set! next 2 previous)))

(define (disconnect-lines! start end)
  (vector-set! start 2 '())
  (vector-set! end 0 '()))


;;; line-length clashes with a scheme-primitive. we have defined
;;; a macro line-length which will replace all occurrences of line-length
;;; to line-string-length. Maybe, we will change it all ove the source
;;; someday. The macro will be present only while compiling Edwin
;;; sources.

;;; (define-integrable (line-length line)
;;;  (string-length (line-string line)))

;;;; Line Numbering

(define line-number-increment 256)

(define (number-lines! start-line end-line)
  (define (number-upward group base increment)
    (define (loop line number)
      (set-line-group! line group)
      (set-line-number! line number)
      (if (not (eq? line end-line))
	  (loop (line-next line) (+ number increment))))
    (loop start-line (+ base increment)))

  (define (number-downward group base increment)
    (define (loop line number)
      (set-line-group! line group)
      (set-line-number! line number)
      (if (not (eq? line start-line))
	  (loop (line-previous line) (- number increment))))
    (loop end-line (- base increment)))

  (define (count-lines)
    (define (loop line n)
      (if (eq? line end-line)
	  n
	  (loop (line-next line) (1+ n))))
    (loop start-line 1))

  (let ((lower-limit (line-previous start-line))
	(upper-limit (line-next end-line)))
    (if (null? lower-limit)
	(if (null? upper-limit)
	    ;; Numbering entire group.  The first line
	    ;; had better be initialized correctly.
	    (number-upward (line-group start-line)
			   0
			   line-number-increment)
	    (number-downward (line-group upper-limit)
			     (line-number upper-limit)
			     line-number-increment))
	(if (null? upper-limit)
	    (number-upward (line-group lower-limit)
			   (line-number lower-limit)
			   line-number-increment)
	    (number-upward (line-group lower-limit)
			   (line-number lower-limit)
			   (/ (- (line-number upper-limit)
				 (line-number lower-limit))
			      (1+ (count-lines))))))))