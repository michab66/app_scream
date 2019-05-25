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


;;;; Incremental Search

;;;; Search State Abstraction

(define search-state-tag "Search State")

(define (make-search-state text parent forward? successful?
			   start-point end-point point initial-point)
  (let ((state (make-vector 9)))
    (vector-set! state 0 search-state-tag)
    (vector-set! state 1 text)
    (vector-set! state 2 parent)
    (vector-set! state 3 forward?)
    (vector-set! state 4 successful?)
    (vector-set! state 5 start-point)
    (vector-set! state 6 end-point)
    (vector-set! state 7 point)
    (vector-set! state 8 initial-point)))

(begin
(define-integrable search-state-index:text         1)
(define-integrable search-state-index:parent       2)
(define-integrable search-state-index:forward?     3)
(define-integrable search-state-index:successful?  4)
(define-integrable search-state-index:start-point  5)
(define-integrable search-state-index:end-point    6)
(define-integrable search-state-index:point        7)
(define-integrable search-state-index:initial-point 8)

(define-integrable search-state-text
  (lambda (search-state)
    (vector-ref search-state search-state-index:text)))

(define-integrable search-state-parent
  (lambda (search-state)
    (vector-ref search-state search-state-index:parent)))

(define-integrable search-state-forward?
  (lambda (search-state)
    (vector-ref search-state search-state-index:forward?)))

(define-integrable search-state-start-point
  (lambda (search-state)
    (vector-ref search-state search-state-index:start-point)))

(define-integrable search-state-end-point
  (lambda (search-state)
    (vector-ref search-state search-state-index:end-point)))

(define-integrable search-state-point
  (lambda (search-state)
    (vector-ref search-state search-state-index:point)))

(define-integrable search-state-initial-point
  (lambda (search-state)
    (vector-ref search-state search-state-index:initial-point)))

(define-integrable search-state-successful?
  (lambda (search-state)
    (vector-ref search-state search-state-index:successful?)))
)
;;;; Top Level


(define (incremental-search forward?)
  (let ((old-point (current-point))
	(old-window (current-window)))
    (let ((y-point (window-point-y old-window)))
      (let ((result
	     (catch
	       (lambda (continuation)
		 (fluid-let ((incremental-search-exit continuation)
			     (incremental-search-window old-window)
			     (current-search-state #!FALSE))
                   (set-current-search-state!
                     (initial-search-state forward? old-point))
                   (incremental-search-loop))))))
	(cond ((eq? result 'ABORT)
	       (set-current-point! old-point)
	       (window-scroll-y-absolute! (current-window) y-point))
	      ((char? result)
               (erase-echo-prompt!)
	       (dispatch-on-char result)))))))

(define (incremental-search-loop)
  (let ((result
	 (catch
	   (lambda (continuation)
	     (fluid-let ((*error-continuation* continuation))
	       (incremental-search-command-reader))))))
    (if (eq? result 'abort)           ;; Handle ^G and go on
	(begin (incremental-search:pop!)
	       (incremental-search-loop))
	result)))

(define ctrl-q (integer->char 17))
(define ctrl-r (integer->char 18))
(define ctrl-s (integer->char 19))

(define (incremental-search-command-reader)
  (let ((char (editor-read-char (window-screen (current-window)))))
    (cond ((standard-char? char) (i-search-append-char char))
          ((char=? char #\Tab) (i-search-append-char char))
          ((char=? char ctrl-q) (i-search-append-char
                                  (read-char (window-screen (current-window)))))
          ((char=? char ctrl-s)
           (set-current-search-state!
             (incremental-search:next-occurrence (fluid current-search-state)))
           (i-search-detect-failure (fluid current-search-state)))
          ((char=? char ctrl-r)
           (set-current-search-state!
             (incremental-search:previous-occurrence
               (fluid current-search-state)))
           (i-search-detect-failure (fluid current-search-state)))
          ((char=? char #\backspace)
           (set-current-search-state!
             (incremental-search:delete-char (fluid current-search-state))))
          (t (incremental-search:terminate! (fluid current-search-state)
                                            char))))
  (incremental-search-command-reader))

(define (standard-char? char)
  (let ((i (char->integer char)))
    (and (>= i 32) (<= i 126))))


;;;; Commands

(define (incremental-search:append-char state char)
  (let ((text (string-append (search-state-text state)
                             (list->string (list char)))))
    (cond ((not (search-state-successful? state))
	   (unsuccessful-search-state state text
				      (search-state-forward? state)))
	  ((search-state-forward? state)
	   (find-next-search-state state
				   text
				   (search-state-start-point state)))
	  (else
	   (find-previous-search-state
	    state text
	    (let ((end (search-state-end-point state)))
	      (if (or (group-end? end)
		      (mark= end (search-state-initial-point state)))
		  end
		  (mark1+ end #!false))))))))

(define (incremental-search:delete-char state)
  (let ((parent (search-state-parent state)))
    (if (null? parent) (editor-error))
    parent))

(define (incremental-search:next-occurrence state)
  (cond ((null? (search-state-parent state))
	 (let ((point (search-state-initial-point state)))
	   (if (not (search-state-forward? state))
	       (initial-search-state #!FALSE point)
               (find-next-search-state state
                 previous-successful-search-string
                 point))))
	((search-state-successful? state)
	 (find-next-search-state state
				 (search-state-text state)
				 ((if (search-state-forward? state)
				      search-state-end-point
				      search-state-start-point)
				  state)))
	((not (search-state-forward? state))
	 (find-next-search-state state
				 (search-state-text state)
				 (search-state-point state)))
	(else
	 (unsuccessful-search-state state (search-state-text state) #!TRUE))))

(define (incremental-search:previous-occurrence state)
  (cond ((null? (search-state-parent state))
	 (let ((point (search-state-initial-point state)))
	   (if (search-state-forward? state)
	       (initial-search-state #!FALSE point)
               (find-previous-search-state state
                  previous-successful-search-string
                  point))))
	((search-state-successful? state)
	 (find-previous-search-state state
				     (search-state-text state)
				     ((if (search-state-forward? state)
					  search-state-end-point
					  search-state-start-point)
				      state)))
	((search-state-forward? state)
	 (find-previous-search-state state
				     (search-state-text state)
				     (search-state-point state)))
	(else
	 (unsuccessful-search-state state (search-state-text state) #!FALSE))))

(define (incremental-search:terminate! state char)
  (let ((state (most-recent-successful-search-state state)))
    (if (not (null? (search-state-parent state)))
	(set! previous-successful-search-string (search-state-text state))))
  ((fluid incremental-search-exit) char))

(define (incremental-search:pop!)
  (let ((success (most-recent-successful-search-state
		   (fluid current-search-state))))
    (if (eq? success (fluid current-search-state))
	((fluid incremental-search-exit) 'ABORT)
	(set-current-search-state! success))))

;;;; Primitives

(define (initial-search-state forward? point)
  (make-search-state "" '() forward? #!TRUE point point point point))

(define (unsuccessful-search-state parent text forward?)
  (let ((start-point (search-state-start-point parent)))
    (make-search-state text parent forward? #!FALSE
		       start-point
		       (mark+ start-point (string-length text) #!false)
		       (search-state-point parent)
		       (search-state-initial-point parent))))

(define (find-next-search-state state text start)
  (let ((start-point (find-next-string start (group-end start) text)))
    (if (not start-point)
	(unsuccessful-search-state state text #!TRUE)
	(let ((end-point (mark+ start-point (string-length text) #!false)))
	  (make-search-state text state #!TRUE #!TRUE
			     start-point end-point end-point
			     (if (search-state-forward? state)
				 (search-state-initial-point state)
				 (search-state-start-point state)))))))

(define (find-previous-search-state state text start)
  (let ((end-point (find-previous-string start (group-start start) text)))
    (if (not end-point)
	(unsuccessful-search-state state text #!FALSE)
	(let ((start-point (mark- end-point (string-length text) #!false)))
	  (make-search-state text state #!FALSE #!TRUE
			     start-point end-point start-point
			     (if (search-state-forward? state)
				 (search-state-end-point state)
				 (search-state-initial-point state)))))))

(define (set-current-search-state! state)
  (update-i-search-prompt state)
  (set-window-point! (fluid incremental-search-window)
		     (search-state-point state))
  (set-fluid! current-search-state state))

(define (update-i-search-prompt state)
  (set-echo-prompt!
    (string-append
      (if (search-state-successful? state) "" "Failing ")
      (if (search-state-forward? state) "" "Reverse ")
      "I-Search: "
      (search-state-text state))))

(define (most-recent-successful-search-state state)
  (cond ((search-state-successful? state)
	 state)
	((null? (search-state-parent state))
	 (error "Search state chain terminated improperly"))
	(else
	 (most-recent-successful-search-state (search-state-parent state)))))

(define (i-search-append-char char)
  (set-current-search-state!
   (incremental-search:append-char (fluid current-search-state) char))
  (i-search-detect-failure (fluid current-search-state)))

(define (i-search-detect-failure search-state)
  (if (and (not (search-state-successful? search-state))
	   (or (search-state-successful? (search-state-parent
                                           search-state))
	       (not (eq? (search-state-forward? search-state)
			 (search-state-forward?
			  (search-state-parent search-state))))))
      (beep)))
