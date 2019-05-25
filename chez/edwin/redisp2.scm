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
;;;     Modified by Texas Instruments Inc 10/21/85
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (window-scroll-y-absolute! window y-point)
  (window-scroll-y-relative! window (- (window-point-y window) y-point)))

(define window-scroll-y-relative!
  (letrec ((%receiver
	     (lambda (w)
	       (let ((buffer (vector-ref w window:buffer))
		     (table  (vector-ref w window:lines)))
		 (set-buffer-point! buffer (window-coordinates->mark w 0 0))
		 (vector-set! w window:point (buffer-point buffer))
		 (cursor-moved! w)))))
     (lambda (window y-delta)
       (cond ((negative? y-delta) (scroll-down-y! window (- y-delta)))
	     ((positive? y-delta) (scroll-up-y! window y-delta)))
       (if (<> y-delta 0)
	   (begin
	    (set-start-end! window 0 (-1+ (vector-ref window window:y-size)))
	    (everything-changed! window %receiver))))))


;;; Scrolling

;;; Scrolling down

(define (scroll-down-y! window y-delta)
  (define (check-y-start y-delta table y-size)
    (let ((y-start (inferior:y-start (vector-ref table y-delta))))
      (if (< y-start y-delta)
	  (let ((y (max 0 y-start)))
	    (fill-entries y y-delta y-delta table y-size)
	    y)
	  y-delta)))

  (let ((table (vector-ref window window:lines))
	(y-size (vector-ref window window:y-size)))
    (if (< y-delta y-size)
	(begin
	 (scroll-lines-down! window y-delta y-size table 0)
	 (let ((y (check-y-start y-delta table y-size)))
	   (fill-top! window (inferior:line (vector-ref table y))
		      table y-size y #!false)))
	(redraw-screen! window
			(line-start
			  (make-mark (inferior:line (vector-ref table 0)) 0)
			  (- 0 y-delta) 'ERROR)
			0))))
(define (scroll-lines-down! window y-delta y-size table y)
  (let loop ((n (-1+ (- y-size y-delta)))
	     (table table))
       (if (< n y)
	   '()
	   (let ((inferior (vector-ref table n)))
	     (if (inferior:line inferior)
		 (begin
		  (set-inferior:line! (vector-ref table (+ n y-delta))
				      #!false)
		  (exchange-inferiors table n (+ n y-delta))))
	     (loop (-1+ n) table)))))


(define (scroll-up-y! window y-delta)
  (let ((table (vector-ref window window:lines))
	(y-size (vector-ref window window:y-size)))
    (if (< y-delta y-size)
	(if (inferior:line (vector-ref table y-delta))
	    (scroll-lines-up! window y-delta y-size table y-delta)
	    '())
	(redraw-screen! window
			(line-start
			  (make-mark (inferior:line (vector-ref table 0)) 0)
			  y-delta 'ERROR)
			0))))

(define (scroll-lines-up! window y-delta y-size table y)
  (define (loop n y-size table)
    (let ((move-to (- n y-delta)))
      (if (or (>= n y-size)
	      (not (inferior:line (vector-ref table n))))
	  (fill-bottom! move-to y-size table
			(inferior:line (vector-ref table (-1+ move-to))))
	  (begin
	    (set-inferior:line! (vector-ref table move-to) #!false)
	    (exchange-inferiors table move-to n)
	    (loop (1+ n) y-size table)))))
  (loop y y-size table))


;;; Fill top and Bottom

(define (fill-top! window %line table y-size n fill-bottom?)
  (define (loop y table line)
    (cond ((< y 0)
	   (if fill-bottom?
               (let ((inferior (vector-ref table n)))
		 (let ((ys (inferior:y-size inferior))
		       (y-start (inferior:y-start inferior)))
		   (fill-bottom! (+ ys y-start) y-size table %line)))))
	  ((null? line)
	   (scroll-lines-up! window (+ y 1) y-size table (+ y 1)))
	  (else
	    (let ((inferior (vector-ref table y)))
	      (update-top-inferior! 0 y line table inferior y-size)
	      (loop (- y (inferior:y-size inferior)) table
		    (line-previous line))))))
  (loop (-1+ n) table (line-previous %line)))

(define (update-top-inferior! x y line table inferior ys)
  (let ((y-size (find-y-size line)))
    (update-inferior! line x (1+ (- y y-size)) y-size inferior)
    (if (> y-size 1)
	(fill-entries (max 0 (1+ (- y y-size))) y y table ys))))


;;; Fill Bottom

(define (fill-bottom! n y-size table line)
  (define (loop n line y-size table)
    (if (< n y-size)
	(let ((inferior (vector-ref table n)))
	  (if (null? line)
	      (begin
		(set-inferior:line! inferior #!false)
		(loop (1+ n) '() y-size table))
	      (begin
		(update-bottom-inferior! line 0 n inferior table y-size)
		(loop (+ n (inferior:y-size inferior)) (line-next line)
		      y-size table))))))
  (loop n (line-next line) y-size table))

(define (update-bottom-inferior! line x y inferior table ys)
  (let ((y-size (find-y-size line)))
    (update-inferior! line x y y-size inferior)
    (if (> y-size 1)
	(fill-entries (1+ y) (min ys (+ y y-size)) y table ys))))

(define (update-inferior! line x y y-size inferior)
  (set-inferior:x-start! inferior x)
  (set-inferior:y-start! inferior y)
  (set-inferior:line! inferior line)
  (set-inferior:y-size! inferior y-size))

;;; Fill enteries

(define (fill-entries start end copy-entry table ys)
  (let ((copy-entry (vector-ref table copy-entry)))
    (do ((x-start (inferior:x-start copy-entry))
	 (y-start (inferior:y-start copy-entry))
	 (y-size  (inferior:y-size copy-entry))
	 (line	(inferior:line copy-entry))
	 (n start (1+ n)))
	((or (>= n ys) (= n end)) #!true)
      (and (>= n 0)
	   (let ((entry (vector-ref table n)))
	     (set-inferior:x-start! entry x-start)
	     (set-inferior:y-start! entry y-start)
	     (set-inferior:y-size! entry y-size)
	     (set-inferior:line! entry line))))))

(define (exchange-inferiors table n1 n2)
  (let ((inferior1 (vector-ref table n1))
	(inferior2 (vector-ref table n2))
	(diff (- n2 n1)))
    (set-inferior:y-start! inferior1
		 (+ diff (inferior:y-start inferior1)))
    (set-inferior:y-start! inferior2
		 (- (inferior:y-start inferior2) diff))
    (vector-set! table n1 inferior2)
    (vector-set! table n2 inferior1)))


(define (clean-up-table table n1 n2)
  (do ((i n1 (1+ i))
       (table table))
      ((= i n2) table)
    (set-inferior:line! (vector-ref table i) #!false)))

(define (find-y-size line)
  (let* ((string (line-string line))
	 (x (char->x string (string-length string))))
    (if (zero? x)
	1
	(let ((q (quotient x 79))
	      (r (remainder x 79)))
	  (if (zero? r)
	      q
	      (1+ q))))))

(define (set-cursor-coordinates window mark)
  (let ((line (mark-line mark))
	(position (mark-position mark))
	(string (line-string (mark-line mark)))
	(x-size (window-x-size window))
	(table (vector-ref window window:lines)))
    (let ((y (inferior:y-start
	       (vector-ref table (line->y window line))))

	  (x (char->x string position)))
      (set-cursor-pos window
		      (index->x x x-size position string)
		      (+ y (index->y x x-size position string))))))



(define (index->x column x-size index string)
  (if (zero? column)
      0
      (let ((r (remainder column (-1+ x-size))))
	(if (zero? r)
	    (if (=? index (string-length string))
		(-1+ x-size)
		r)
	    r))))

(define (index->y column x-size index string)
  (if (zero? column)
      0
      (let ((q (quotient column (-1+ x-size)))
	    (r (remainder column (-1+ x-size))))
	(if (zero? r)
	    (if (=? index (string-length string))
		(-1+ q)
		q)
	    q))))


(define make-insert-daemon
  (lambda (window)
    (letrec
      ((%receiver
	 (lambda (region)
	   (region-components region
	     (lambda (start-line start-position end-line end-position)
	       (let* ((table (vector-ref window window:lines))
		      (inferior (vector-ref table y)))
		 (let ((y-size (vector-ref window window:y-size))
		       (old-ys (inferior:y-size inferior))
		       (new-ys (find-y-size start-line)))
		   (cond
		    ((eq? start-line end-line)
		     (if (= old-ys new-ys)
			 (begin
			  (maybe-marks-changed window y)
			  (set-start-end! window y y)
			  (cursor-moved! window))
			 (begin
			  (scroll-lines-down! window (- new-ys old-ys)
                                  y-size table 
				  (+ (inferior:y-start inferior) old-ys))
			  (set-inferior:y-size! inferior new-ys)
			  (fill-entries (1+ y) 
					(+ (inferior:y-start inferior) new-ys)
					y table y-size)
			  (set-start-end! window y (-1+ y-size))
			  (everything-changed! window window-redraw!))))
		    (else
		      (update-bottom-inferior! start-line 0 y
					       inferior table y-size)
		      (fill-bottom! (+ y new-ys) y-size table start-line)
		      (set-start-end! window y (-1+ y-size))
		      (everything-changed! window window-redraw!)))))))))

       (y '()))
    (lambda (mark)
      (if (line-visible? window mark)
	  (begin
	    (set! y (line->y window (mark-line mark)))
	    %receiver))))))


(define set-start-end!
  (lambda (window start end)
    (if (vector-ref window window:redisplay-window-flag)
	(begin
	  (vector-set! window window:start
		       (min start (vector-ref window window:start)))
	  (vector-set! window window:end
		       (max end (vector-ref window window:end))))
	(begin
	  (vector-set! window window:start start)
	  (vector-set! window window:end end)))
     (vector-set! window window:redisplay-window-flag #!TRUE)))



(define make-delete-daemon
  (lambda (window)
    (letrec
      ((start-y '())
       (end-y '())
       (mark '())
  (%receiver
    (lambda (region)
      (let ((table (vector-ref window window:lines))
	    (line  (mark-line mark))
	    (y-size (vector-ref window window:y-size)))
	(set! mark '())                     ;; clean up
	(cond ((not start-y)		    ;;; deleted top
	       (cond ((not end-y)
		      (window-redraw! window))
		     (else
		      (clean-up-table table 0 y-size)
		      (update-bottom-inferior! line 0 end-y
			     (vector-ref table end-y) table y-size)
		      (fill-top! window line table y-size end-y #!true)
		      (set-start-end! window 0 (-1+ y-size))
		      (everything-changed! window window-redraw!))))
	      ((and end-y (=? start-y end-y))
	       (let ((inferior (vector-ref table start-y)))
		 (let ((old-ys (inferior:y-size inferior))
		       (new-ys (find-y-size line))
		       (y start-y))
		   (if (= old-ys new-ys)
		       (begin
			(maybe-marks-changed window y)
			(set-start-end! window y y)
			(cursor-moved! window))
		       (begin
			(scroll-lines-up! window (- old-ys new-ys)
                                y-size table 
			        (+ (inferior:y-start inferior) old-ys))
			(set-inferior:y-size! inferior new-ys)
			(fill-entries (1+ y) 
				      (+ (inferior:y-start inferior) new-ys)
				      y table y-size)
			(set-start-end! window y (-1+ y-size))
			(everything-changed! window window-redraw!))))))
	      (else
	       (let ((inferior (vector-ref table start-y)))
		 (let ((ys (find-y-size line))
		       (y start-y))
		   (update-bottom-inferior! line 0 y inferior table y-size)
		   (fill-bottom! (+ y ys) y-size table line)
		   (set-start-end! window y (-1+ y-size))
		   (everything-changed! window window-redraw!)))))))))

    (lambda (region)
      (let ((start (region-start region))
	    (end (region-end region)))
	(let ((*line (mark-line start))
	      (*pos  (mark-position start)))
	  (set! start-y (line->y window *line))
	  (set! end-y (line->y window (mark-line end)))
	  (set! mark (if (and start-y end-y (= start-y end-y))
			 start
			 (mark-permanent! start)))
	  %receiver))))))





(define direct-output-for-insert!
  (lambda (window char)
    (let ((x (vector-ref window window:cursor-x))
	  (y (vector-ref window window:cursor-y))
	  (screen (vector-ref window window:screen)))
      (maybe-marks-changed window y)
      (write-string! screen char x y )
      (vector-set! window window:cursor-x
		   (1+ x)))))

(define direct-output-forward-character!
  (lambda (window)
    (let ((screen (vector-ref window window:screen))
	  (buffer (vector-ref window window:buffer))
	  (point  (vector-ref window window:point))
	  (x (vector-ref window window:cursor-x)))
      (set-buffer-point! buffer (mark1+ point #!false))
      (vector-set! window window:point (buffer-point buffer))
      (%reify-port! screen screen:cursor-x (1+ x))
      (vector-set! window window:cursor-x (1+ x)))))

(define direct-output-backward-character!
  (lambda (window)
    (let ((screen (vector-ref window window:screen))
	  (buffer (vector-ref window window:buffer))
	  (point  (vector-ref window window:point))
	  (x (vector-ref window window:cursor-x)))
      (set-buffer-point! buffer (mark-1+ point #!false))
      (vector-set! window window:point (buffer-point buffer))
      (%reify-port! screen screen:cursor-x (-1+ x))
      (vector-set! window window:cursor-x (-1+ x)))))


