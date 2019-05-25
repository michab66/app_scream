;
; This is an example of using SCOOPS. Please refer to chapter 5 in the
; Language Reference Manual for TI Scheme. 
;
; The first thing that needs to be done is to define classes for different
; types. We will define three types, points, lines and rectangles.

(load "scoops.fsl")

;;;
;;; Point, Line and Rectangle
;;;

;;;
;;; Class POINT
;;;

(define-class point
	      (instvars (x     (active 0       () move-x))
			(y     (active 0       () move-y))
			(color (active 'yellow () change-color)))
	      (options	settable-variables
			inittable-variables))

(compile-class point)	      ; see page 45 in the language reference manual

;;;
;;; Class LINE
;;;

(define-class line
	      (instvars (len (active 50 () change-length))
			(dir (active 0	() change-direction)))
	      (mixins point)  ; inherit x, y, and color from point class.
	      (options settable-variables))

(compile-class line)

;;;
;;; Class RECTANGLE
;;;

(define-class rectangle
	      (instvars (height (active 60 () change-height)))
	      (mixins line)  ; inherit color and width (len) from line
	      (options settable-variables))

(compile-class rectangle)

; In order to have an occurance of a class you will need to use the
; MAKE-INSTANCE procedure. For example:
;     (define p1 (make-instance point))
; Then to change parts of the class use the send function. For example
; to change the color of the point previously defined:
;     (send p1 change "color" 'cyan)
;

;;;
;;; Methods for POINT
;;;

(define-method (point erase) ()
	       (set-pen-color! 'black)
	       (draw))

(define-method (point draw) ()
	       (draw-point x y))

; having both a draw and redraw function here may seem to be unnecessary.
; you will see why both are needed as we continue

(define-method (point redraw) ()
	       (set-pen-color! color)
	       (draw))

(define-method (point move-x) (new-x)
	       (erase)
	       (set! x new-x)
	       (redraw)
	       new-x)

(define-method (point move-y) (new-y)
	       (erase)
	       (set! y new-y)
	       (redraw)
	       new-y)

(define-method (point change-color) (new-color)
	       (erase)
	       (set! color new-color)
	       (redraw)
	       new-color)
;;;
;;; Methods for LINE
;;;

; inherit erase, redraw, move-x, move-y and change-color from point.

(define-method (line draw) ()
	       (position-pen x y)
	       (draw-line-to (truncate (+ x (* len (cos dir))))
			     (truncate (+ y (* len (sin dir))))))

(define-method (line change-length) (new-length)
	       (erase)
	       (set! len new-length)
	       (redraw)
	       new-length)

(define-method (line change-direction) (new-dir)
	       (erase)
	       (set! dir new-dir)
	       (redraw)
	       new-dir)

;;;
;;; Methods for RECTANGLE
;;;

; inherit erase, redraw, move-x, move-y and change-color from point.

(define-method (rectangle draw) ()
	       (position-pen x y)
	       (draw-line-to (+ x len) y)
	       (draw-line-to (+ x len) (+ y height))
	       (draw-line-to x (+ y height))
	       (draw-line-to x y))

(define-method (rectangle change-height) (new-height)
	       (erase)
	       (set! height new-height)
	       (redraw)
	       new-height)

