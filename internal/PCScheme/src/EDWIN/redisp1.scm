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


;;; define-integrables
(begin
(define-integrable inferior:x-start cddr)
(define-integrable inferior:y-start cadr)
(define-integrable inferior:line caar)
(define-integrable inferior:y-size cdar)
(define-integrable set-inferior:x-start!
  (lambda (inferior val)
    (set-cdr! (cdr inferior) val)))

(define-integrable set-inferior:y-start!
  (lambda (inferior val)
    (set-car! (cdr inferior) val)))

(define-integrable set-inferior:line!
  (lambda (inferior val)
    (set-car! (car inferior) val)))

(define-integrable set-inferior:y-size!
  (lambda (inferior val)
    (set-cdr! (car inferior) val)))


(define-integrable screen:cursor-y 0)
(define-integrable screen:cursor-x 1)
(define-integrable screen:x-size 5)
(define-integrable screen:y-size 4)
(define-integrable window:point 0)
(define-integrable window:lines 1)
(define-integrable window:map 2)
(define-integrable window:screen 3)
(define-integrable window:y-size 4)
(define-integrable window:start 5)
(define-integrable window:end 6)
(define-integrable window:buffer 7)
(define-integrable window:cursor-x 8)
(define-integrable window:cursor-y 9)
(define-integrable window:redisplay-window-flag 10)
(define-integrable window:redisplay-cursor-flag 11)
(define-integrable window:start-mark 12)
(define-integrable window:end-mark 13)
(define-integrable window:last-inferior-y 14)

(define-integrable window-point
  (lambda (window)
    (vector-ref window window:point)))

(define-integrable window-point-x
  (lambda (window)
    (vector-ref window window:cursor-x)))

(define-integrable window-point-y
  (lambda (window)
    (vector-ref window window:cursor-y)))

(define-integrable window-buffer
  (lambda (window)
    (vector-ref window window:buffer)))

(define-integrable window-screen
  (lambda (window)
    (vector-ref window window:screen)))

(define-integrable window-y-size
  (lambda (window)
    (vector-ref window window:y-size)))

(define-integrable window-x-size
  (lambda (window)
     80))
)

(define update-cursor!
  (lambda (window)
    (let ((screen (vector-ref window window:screen))
	  (x (vector-ref window window:cursor-x))
	  (y (vector-ref window window:cursor-y)))
      (vector-set! window window:redisplay-cursor-flag #!false)
      (if (and (not (negative? x))
	       (not (negative? y)))
          (set-screen-cursor! screen x y)))))


(define (set-screen-cursor! screen x y)
  (%reify-port! screen screen:cursor-x x)
  (%reify-port! screen screen:cursor-y y))

(define set-cursor-pos
  (lambda (window x y)
    (vector-set! window window:cursor-x x)
    (vector-set! window window:cursor-y y)
    (vector-set! window window:redisplay-cursor-flag #!true)))

(define write-string!
  (lambda (screen string x y)
    (set-screen-cursor! screen x y)
    (princ string screen)))




(define (make-buffer-window screen buffer)
 (define (setup-inferior-table table y-size)
   (do ((i 0 (1+ i))
	(table table))
       ((= i y-size) table)
     (vector-set! table i (cons (cons #!false #!false) (cons i 0)))))

 (define initialize!
   (lambda (window buffer)
     (add-buffer-window! buffer window)
;;;; this is for the speed up hack insertch.scm
     (%create-char-daemon window)
     (let ((group (buffer-group buffer)))
       (add-group-delete-daemon! group (make-delete-daemon window))
       (add-group-insert-daemon! group (make-insert-daemon window)))
     (vector-set! window window:point (buffer-point buffer))))

  (let ((window (make-vector 15 #!false))
	(start-buffer (buffer-start buffer))
	(y-size (%reify-port screen screen:y-size)))
    (let ((table (setup-inferior-table (make-vector y-size) y-size)))
      (vector-set! window window:y-size y-size)
      (vector-set! window window:lines table)
      (vector-set! window window:screen screen)
      (vector-set! window window:buffer buffer)
      (update-bottom-inferior! (mark-line start-buffer) 0 0
			       (vector-ref table 0) table y-size)
      (map-changed! window)
      (vector-set! window window:start 0)
      (vector-set! window window:end 0)
      (vector-set! window window:cursor-x 0)
      (vector-set! window window:cursor-y 0)
      (vector-set! window window:start-mark start-buffer)
      (vector-set! window window:end-mark start-buffer)
      (vector-set! window window:last-inferior-y 0)
      (initialize! window buffer)
      window)))



(define window-y-size-changed
  (lambda (window)
    (vector-set! window window:y-size
		 (%reify-port (vector-ref window window:screen)
			      screen:y-size))
    (vector-set! window window:map '())
    (window-redraw! window)))


(define line->y
  (lambda (window line)
    (let ((entry (assq line (vector-ref window window:map))))
      (and entry
	   (cdr entry)))))


(define set-window-point!
  (lambda (window mark)
    (let ((buffer (vector-ref window window:buffer)))
      (set-buffer-point! buffer mark)
      (vector-set! window window:point (buffer-point buffer))
      (cursor-moved! window))))


(define cursor-moved!
  (lambda (window)
    (let ((point (vector-ref window window:point)))
      (if (window-mark-visible? window point)
	  (set-cursor-coordinates window point)
	  (window-redraw! window)))))


(define (map-changed! window)
  (define (loop tail n table y-size)
    (if (or (>= n y-size)
	    (null? (inferior:line (vector-ref table n))))
	tail
	(let ((inferior (vector-ref table n)))
	  (loop (cons (cons (inferior:line inferior) n)
		      tail)
		(+ (inferior:y-start inferior) (inferior:y-size inferior))
		table y-size))))
  (let ((map (loop '() 0 (vector-ref window window:lines)
		   (vector-ref window window:y-size))))
    (vector-set! window window:map map)
    (vector-set! window window:last-inferior-y (cdar map))))

(define clear-subscreen!
  (lambda (screen xl yl lin col)
    (let ((sxl	(%reify-port screen 3))
	  (syl	(%reify-port screen 2))
	  (slin (%reify-port screen 4))
	  (scol (%reify-port screen 5))
	  (change-cord
	     (lambda (x y l c)
	       (%reify-port! screen 3 x)
	       (%reify-port! screen 2 y)
	       (%reify-port! screen 4 l)
	       (%reify-port! screen 5 c))))
      (change-cord (+ sxl xl) (+ syl yl) lin col)
      (%clear-window screen)
      (change-cord sxl syl slin scol))))

(define (redisplay window table start end)
  (let loop ((screen (window-screen window)) (n start) (end end)
	     (table table) (y-size (vector-ref window window:y-size)))
       (if (> n end)
	  '()
	  (let ((inferior (vector-ref table n)))
	    (if (inferior:line inferior)
		(begin
		 (let ((y-start (inferior:y-start inferior))
		       (ys  (inferior:y-size inferior))
		       (string (line-string (inferior:line inferior))))
		   (set-screen-cursor! screen 0 (max 0 y-start))
		   (%substring-display string 0 (string-length string) y-start
				    screen)
		   (loop screen (+ y-start ys) end table y-size)))
		(clear-subscreen! screen 0 n (1+ (- end n)) 80))))))


(define update-window!
  (lambda (window)
    (let ((table (vector-ref window window:lines))
	  (start (vector-ref window window:start))
	  (end	 (vector-ref window window:end)))
      (redisplay window table start end)
      (vector-set! window window:redisplay-window-flag #!false))))

(define update-display!
  (lambda (window)
    (if (vector-ref window window:redisplay-window-flag)
	(update-window! window))
    (if (vector-ref window window:redisplay-cursor-flag)
	(update-cursor! window))))


(define reset-buffer-window
  (lambda (window)
    (vector-set! window window:start 0)
    (vector-set! window window:end
		 (-1+ (vector-ref window window:y-size)))
    (vector-set! window window:redisplay-window-flag #!true)
    (vector-set! window window:redisplay-cursor-flag #!true)
    (update-display! window)))


;;; redisp2

(define window-redraw!
  (letrec ((%receiver (lambda (w) (error "window-redraw"))))
    (lambda (window)
      (let ((mark (vector-ref window window:point))
	    (y (quotient (vector-ref window window:y-size) 2)))
	(set-start-end! window 0 (-1+ (vector-ref window window:y-size)))
	(redraw-screen! window mark y)
	(everything-changed! window %receiver)))))

(define redraw-screen!
  (lambda (window mark y)
    (let ((line (mark-line mark))
	  (table (vector-ref window window:lines))
	  (y-size (vector-ref window window:y-size))
	  (position (mark-position mark))
	  (string (line-string (mark-line mark))))
      (let ((y* (index->y (char->x string position) 80 position string)))
	(let ((start (max 0 (- y y*)))
	      (ys (find-y-size line))
	      (y-start (- y y*)))
	(clean-up-table table 0 y-size)
	(update-inferior! line 0 y-start ys (vector-ref table start))
	(if (> ys 1)
	    (fill-entries (1+ start) (min y-size (+ y-start ys))
			  start table y-size))
	(fill-top! window line table y-size start #!TRUE))))))

(define everything-changed!
  (lambda (window if-not-visible)
    (map-changed! window)
    (start-mark-changed! window)
    (end-mark-changed! window)
    (if (window-mark-visible? window (vector-ref window window:point))
	(begin
	  (cursor-moved! window))
	(if-not-visible window))))

(define (window-mark-visible? window mark)
  (and (mark<= (vector-ref window window:start-mark) mark)
       (mark<= mark (vector-ref window window:end-mark))))

(define (line-visible? window point)
  (assq (mark-line point)
	(vector-ref window window:map)))


;;; coordinates

(define window-coordinates->mark
  (lambda (window x y)
    (let* ((table (vector-ref window window:lines))
	   (inferior (vector-ref table y)))
      (make-mark (inferior:line inferior)
		 (x->char (line-string (inferior:line inferior))
			  (+ x (* (- y (inferior:y-start inferior)) 79)))))))

(define (start-mark-changed! window)
  (vector-set! window window:start-mark
	       (window-coordinates->mark window 0 0)))

(define (end-mark-changed! window)
  (let ((inferior (vector-ref (vector-ref window window:lines)
			      (vector-ref window window:last-inferior-y)))
	(y-size (vector-ref window window:y-size)))
    (let ((line (inferior:line inferior))
	  (y-start (inferior:y-start inferior))
	  (ys  (inferior:y-size inferior)))
      (vector-set! window window:end-mark
	(make-mark
	  line
	  (end-column->index
	   (line-string line)
	   (+ 79 (* (- (-1+ (min y-size (+ y-start ys))) y-start) 79))))
	))))


(define (maybe-marks-changed window y)
  (if (= y 0)
      (start-mark-changed! window))
  (if (= y (vector-ref window window:last-inferior-y))
      (end-mark-changed! window)))




;;; index->column


(define (char->x string char-no)
  (let loop ((start 0)(tot 0)(end char-no)(string string))
       (let ((index (substring-find-next-char-in-set string start end
						     non-graphic-chars)))
	 (if index
	     (let ((tot (+ tot (- index start))))
	       (loop (1+ index)
		     (+ tot (if (char-ci=? #\tab (string-ref string index))
				(- 8 (remainder tot 8))
				2))
		     end string))
	     (+ tot (- end start))))))

;;; column->index


(define (x->char string column)
  (let loop ((string string)(start 0)(c 0)(end (string-length string))
	     (column column))
       (let ((i (substring-find-next-char-in-set string start end
						 non-graphic-chars)))
	 (if i
	     (let ((new-c (+ c (- i start))))
	       (if (<= column new-c)
		   (+ start (- column c))
		   (let ((new-c (+ new-c
				   (if (char-ci=? #\tab (string-ref string i))
				       (- 8 (remainder new-c 8))
				       2))))
		     (if (<= column new-c)
			 (1+ i)
			 (loop string (1+ i) new-c end column)))))
	     (min (+ start (- column c)) end)))))



(define (end-column->index string column)
  (let loop ((string string)(start 0)(c 0)(end (string-length string))
	     (column column))
       (let ((i (substring-find-next-char-in-set string start end
						 non-graphic-chars)))
	 (if i
	     (let ((new-c (+ c (- i start))))
	       (if (<= column new-c)
		   (+ start (- column c))
		   (let ((new-c (+ new-c
				   (if (char-ci=? #\tab (string-ref string i))
				       (- 8 (remainder new-c 8))
				       2))))
		     (cond ((<? column new-c) i)
			   ((=? column new-c)
			    (if (=? 1 (- end i)) (1+ i) i))
			   (else (loop string (1+ i) new-c end column))))))
             (let ((i (+ start (- column c))))
               (cond ((<? end i) end)
                     ((=? end i) end)
                     (else (-1+ i))))))))






