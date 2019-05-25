(define-integrable setp
  (lambda (x y color xor)
    (%graphics 1 x y color 0 0 xor)))
(define-integrable resetp
  (lambda (cc nc)
    (%graphics 2 cc nc 0 0 0 0)))
(define-integrable line
  (lambda (x1 y1 x2 y2 color xor)
    (%graphics 3 x1 y1 x2 y2 color xor)))
(define-integrable point
  (lambda (x y)
    (%graphics 4 x y 0 0 0 0)))
(define-integrable draw-box
  (lambda (x1 y1 x2 y2 color xor)
    (%graphics 6 x1 y1 x2 y2 color xor)))
(define-integrable draw-filled-box
  (lambda (x1 y1 x2 y2 color xor)
    (%graphics 7 x1 y1 x2 y2 color xor)))
(define-integrable clipping-rectangle
  (lambda (x1 y1 x2 y2)
    (%graphics 8 x1 y1 x2 y2 0 0)))
;
; x and y are coordinates of upper left corner of picture
; a and b are coordinates of upper left corner of clipping rectangle
; c and d are coordinates of lower right corner of clipping rectangle
;
(define cga-example
  (lambda (x y a b c d)
    ; set video mode to graphics
    (set-video-mode! 4)
    (ti-example x y a b c d)
    (display "Type a key to return to mode 3")
    (read-char 'console)
    ; return to text mode
    (set-video-mode! 3)))
(define ega-example
  (lambda (x y a b c d)
    ; set video mode to graphics
    (set-video-mode! 16)
    (ti-example x y a b c d)))
(define ti-example
  (lambda (x y a b c d)
    (clear-graphics)
    ; set clipping rectangle
    (clipping-rectangle a b c d)
    ; draw box (replace)
    (draw-box (+ x 10) (+ y 20) (+ x 50) (+ y 50) 3 0)
    ; draw filled box (exclusive or)
    (draw-filled-box (+ x 30) (+ y 30) (+ x 90) (+ y 120) 2 1)
    ; draw line (exclusive or)
    (line (+ x 10) (+ y 20) (+ x 90) (+ y 120) 1 1)
    ; set point
    (setp (+ x 20) (+ y 20) 2 0)
    ; set palette
    (resetp 2 6)
    ; read color of point
    (point (+ x 20) (+ y 20))))
