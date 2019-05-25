;;; Sample graphics routines using the %GRAPHICS primitive.

;;; Note that %GRAPHICS may change in meaning in future versions of the system,
;;; as it has between versions 2.0 and 3.0.
;;; Using macros or define-integrables to protect your code
;;; from explicit uses of %GRAPHICS is highly recommended.

;;; Determine what type of video adapter we have.
(define video-type
  (lambda ()
    (if (= pcs-machine-type 1)
	;; it's TI
	'ti
	;; it's IBM
	(let ((mode (%graphics 5 0 0 0 0 0 0))) ;; get video mode
	  (case mode
	    (3 'cga)
	    ((14 16) 'ega)
	    (else 'cga))))))


;;; Initialize Graphics (sets palette registers; clears graphics planes)
(define grinit
  (lambda ()
    (case (video-type)
      (ti  (%graphics 0 0 0 0 0 0 0)		;; clear graphics
	   (window-clear (make-window "" '())))
      (cga (%graphics 0 4 0 0 0 0 0)		;; 4-color graphics mode
	   (%graphics 2 0 0 0 0 0 0)		;; set background to black
	   (%graphics 2 1 0 0 0 0 0))		;; use black,red,green,brown
      (ega (%graphics 0 16 0 0 0 0 0)		;; 16-color graphics mode
	   (%graphics 2 0 0 0 0 0 0)		;; not necessary here
	   (%graphics 2 1 0 0 0 0 0))
      )))


;     Set point
(define-integrable setp
  (lambda (x y color) (%graphics 1 x y color 0 0 0)))

;     Reset point (turns it off)
(define-integrable resetp
  (lambda (x y) (%graphics 2 x y 0 0 0 0)))

;     Draw Line
(define-integrable line
  (lambda (x1 y1 x2 y2 color)
    (%graphics 3 x1 y1 x2 y2 color 0)))

;     Read Point (returns its color)
(define-integrable point
  (lambda (x y) (%graphics 4 x y 0 0 0 0)))

; %graphics 5 is identical to get-video-mode

;     Draw box
(define-integrable draw-box
  (lambda (x1 y1 x2 y2 color)
    (%graphics 6 x1 y1 x2 y2 color 0)))

;     Draw Filled Box
(define-integrable draw-filled-box
  (lambda (x1 y1 x2 y2 color)
    (%graphics 7 x1 y1 x2 y2 color 0)))


;     Kaleidoscope Program [Translated from Basic]

; Note: To stop this program, press the "q" key.  To start a new pattern
;	going, press any other key.
(alias kldscope kald)
(alias kaleidosope kald)
(define kald
  (lambda ()
    (let* ((old-video-mode (%graphics 5 0 0 0 0 0 0))
	   (vmode (video-type))
	   (accel-range    (case vmode (ti  12) (cga   6) (ega	12)))
	   (accel-adj	   (case vmode (ti   5) (cga   3) (ega	 5)))
	   (usable-colors  (case vmode (ti   7) (cga   3) (ega	15)))
	   (wh		   (case vmode (ti 360) (cga 160) (ega 320)))
	   (mi		   (case vmode (ti 145) (cga  75) (ega 150)))
	   (ycenter-offset (case vmode (ti   5) (cga  25) (ega	25)))
		;; Add 5/25/25 (TI/CGA/EGA) to y-coordinates 'cause we said that the
		;; screens are only 290/150/300-pixels high when, in actuality,
		;; they're 300/200/350.
	   (m1 (+ mi 1))
	   (xv1 nil)
	   (xv2 nil)
	   (yv1 nil)
	   (yv2 nil)
	   )
	  (letrec
	    (
	     (quit-kald
	       (lambda ()
		 (grinit)
		 (%graphics 0 old-video-mode 0 0 0 0 0)
		 (window-set-cursor! 'console 0 0)
		 (gc)
		 *the-non-printing-object*
		 ))
	     (loop
	       (lambda (a n color x1 y1 x2 y2)
		 (cond ((positive? a)
			(let ((2x1 (+ x1 x1))
			      (2y1 (+ y1 y1))
			      (2x2 (+ x2 x2))
			      (2y2 (+ y2 y2))
			      (w wh)
			      (m (+ mi ycenter-offset)))
		    (line (+ w 2x1) (- m y1) (+ w 2x2) (- m y2) color) ; 1
		    (line (- w 2y1) (+ m x1) (- w 2y2) (+ m x2) color) ; 2
		    (line (- w 2x1) (- m y1) (- w 2x2) (- m y2) color) ; 3
		    (line (- w 2y1) (- m x1) (- w 2y2) (- m x2) color) ; 4
		    (line (- w 2x1) (+ m y1) (- w 2x2) (+ m y2) color) ; 5
		    (line (+ w 2y1) (- m x1) (+ w 2y2) (- m x2) color) ; 6
		    (line (+ w 2x1) (+ m y1) (+ w 2x2) (+ m y2) color) ; 7
		    (line (+ w 2y1) (+ m x1) (+ w 2y2) (+ m x2) color) ; 8
		    (if (positive? n)
			(loop (- a 1)
			      (- n 1)
			      color
			      (remainder (+ x1 xv1) m1)
			      (remainder (+ y1 yv1) m1)
			      (remainder (+ x2 xv2) m1)
			      (remainder (+ y2 yv2) m1))
			(restart))))
		 ((not (char-ready?))
		  (set! xv1 (- (random accel-range) accel-adj))
		  (set! yv1 (- (random accel-range) accel-adj))
		  (set! xv2 (- (random accel-range) accel-adj))
		  (set! yv2 (- (random accel-range) accel-adj))
		  (loop (random 10) n (+ (random usable-colors) 1) x1 y1 x2 y2))
		 ((eq? (char-upcase (read-char)) '#\Q)
		  (quit-kald))
		 (else
		  (restart)))))
	(restart
	 (lambda ()
	   (grinit)
	   (randomize 0)
	   (loop 0 (+ 50 (random 200)) 0
		   (+ (random mi) 1)
		   (+ (random mi) 1)
		   (+ (random mi) 1)
		   (+ (random mi) 1)))))
       (begin
	 (flush-input)
	 (restart))))))
