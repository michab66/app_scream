
;      -*- Mode: Lisp -*-			       Filename:  pgr.s

;		      Last Revision:  7-May-87

;--------------------------------------------------------------------------;
;									   ;
;			  TI SCHEME -- PCS Compiler			   ;
;	        Copyright 1985, 1986 (c) Texas Instruments 		   ;
;									   ;
;		       David Bartley, Rusty Haddock			   ;
;									   ;
;		     MIT-Compatible Graphics Routines			   ;
;									   ;
;--------------------------------------------------------------------------;

; Revisions:
;  ds - added support for EGA modes 14, 16
;  rb  11/5/86 - modified for clipping
;  rb  11/17/86 - graphics windows (they don't remember their state, though)
;  mrm  5/07/87 - special handling for setting mode 3
;  ttc  3/11/88 - added support for VGA mode 18 

(begin
  (define clear-graphics)
  (define clear-point)
  (define draw-point)
  (define draw-line-to)
  (define is-point-on?)
  (define position-pen)
  (define set-pen-color!)
  (define set-video-mode!)
  (define get-video-mode)
  (define draw-box-to)
  (define draw-filled-box-to)
  (define set-palette!)
  (define point-color)                    ;new with 3.0
  (define set-clipping-rectangle!)        ;      "
  (define graphics-window)                ;      "
  (define get-pen-position)               ;      "
  (define get-pen-color)                  ;      "
  (define current-graphics-window)        ;      "
  (define reset-graphics)                 ;not documented 
  )

;;; A small note about the global variable PCS-MACHINE-TYPE:
;;;
;;; PCS-MACHINE-TYPE = 0     Machine type unknown
;;;		     = 1     TIPC	-or-  TI Bus-Pro in TIPC mode
;;;		     = 252   IBM-PC/AT
;;;		     = 253   IBM-PC/jr
;;;		     = 254   IBM-PC/XT
;;;		     = 255   IBM-PC	-or-  TI  Bus-Pro in PC/AT mode
;;;
;;; No variable CURRENTLY indicates whether or not the PC has
;;; bit-mapped graphics capabilities.  (This would be nice though.)

(define *graphics-colors*				; *GRAPHICS-COLORS*
  (if (=? pcs-machine-type 1)
      '((black . 0) (blue . 1) (red . 2) (magenta . 3)
	(green . 4) (cyan . 5) (yellow . 6) (white . 7))
      '((black . 0) (cyan . 1) (magenta . 2) (white . 3)))) ; IBM mode #4

(define *character-boxes*          ; horiz x vert by graphics mode
  '((TI 9 . 12) (4 8 . 8) (14 8 . 8) (16 8 . 14) (18 8 . 16)))


;;; extended MIT Graphics Procedures
;;;
;;; TI User coordinates:  -360 <= X <= +359
;;;			  -150 <= Y <= +149
;;; IBM User coordinates: -160 <= X <= +159  For 320x200/4-color mode  (#4)
;;;			  -100 <= Y <=	+99
;;; IBM User coordinates: -320 <= X <= +319  For 640x200/16-color mode (#14)
;;;			  -100 <= Y <=	+99
;;; IBM User coordinates: -320 <= X <= +319  For 640x350/16-color mode (#16)
;;;			  -175 <= Y <= +174
;;; IBM User coordinates: -320 <= X <= +319  For 640x480/16-color mode (#18)
;;;			  -240 <= Y <= +239
;;;
;;;   for IBM, mode 4 values are the default.
;;;

(let ((cur-x '())     ; X,Y should be in fixnum range, else get
      (cur-y '())     ; "invalid operand" error when %GRAPHICS executes
      (cur-w 'screen)    ; use 'screen for screen, else have window here
                         ; note 'screen and 'console are *not* synonyms
      (cur-color '())
      (max-x (if (=? pcs-machine-type 1) 719 319))
      (max-y (if (=? pcs-machine-type 1) 299 199))
      (mid-x (if (=? pcs-machine-type 1) 360 160))
      (mid-y (if (=? pcs-machine-type 1) 149  99))
      (min-x 0)
      (min-y 0)
      (num-clrs (if (=? pcs-machine-type 1) 8 4)))
  
  (begin

    (if (=? pcs-machine-type 1)
	(set! clear-graphics			; CLEAR-GRAPHICS (TIPC)
	      (lambda ()
                (reset-graphics)
                (if (not (eq? cur-w 'screen))
                    (begin
                      (graphics-window cur-w)
                      (position-pen 0 0)
                      (%graphics 7 0 0 1024 1024 0 0))  ; clear window to black
                    (begin
                      (%graphics 0 0 0 0 0 0 0)   ; Clear the graphics planes
                      (%graphics 0 3 0 0 0 0 0))) ; Enable both text & graphics planes
		'()))

	(set! clear-graphics			; CLEAR-GRAPHICS (IBM)
              (lambda ()
                (reset-graphics)
                (if (not (eq? cur-w 'screen))
                    (begin
                      (graphics-window cur-w)
                      (position-pen 0 0)
                      (%graphics 7 0 0 1024 1024 0 0)) ; clear window to black
                    (%graphics 0 (get-video-mode)
			   0 0 0 0 0)) ; IBM graphics and text are on same
		                       ; plane and will SCROLL together!!!
		(%graphics 2 1 1 0 0 0 0) ; Ensure proper colors are used - CGA
                '())))

    (set! reset-graphics
          (lambda ()
            (if (=? pcs-machine-type 1)
                (begin                    ;TI
                  (set! max-x 719)
                  (set! max-y 299)
                  (set! mid-x 359)
                  (set! mid-y 149)
                  (set! min-x 0)
                  (set! min-y 0)
                  (set! cur-color 7)
                  (position-pen 0 0))
                (case (get-video-mode)    ;IBM
                  (4
                    (set! max-x 319)
                    (set! max-y 199)
                    (set! mid-x 160)
                    (set! mid-y 99)
                    (set! min-x 0)
                    (set! min-y 0)
                    (set! num-clrs 4)
                    (set! *graphics-colors*
                          '((black . 0) (cyan . 1) (magenta . 2) (white . 3)))
                    (set! cur-color (sub1 num-clrs))
                    (position-pen 0 0))
                  ((14 16 18)
                   (set! max-x 639)
                   (set! mid-x 320)
                   (set! min-x 0)
                   (set! min-y 0)
                   (set! num-clrs 16)
                   (set! *graphics-colors*
                         '((black . 0) (blue . 1) (green . 2) (cyan . 3)
                           (red . 4) (magenta . 5) (brown . 6) (white . 7)
                           (gray . 8) (light-blue . 9)
                           (light-green . 10) (light-cyan . 11)
                           (light-red . 12) (light-magenta . 13)
                           (yellow . 14) (intense-white . 15)))
                   (set! cur-color (sub1 num-clrs))
                   (case (get-video-mode)
                     (14
                       (set! max-y 199)
                       (set! mid-y 99))
                     (16
                       (set! max-y 349)
                       (set! mid-y 174))
                     (18
                       (set! max-y 479)
                       (set! mid-y 238)))
                   (position-pen 0 0))
                  (else
                    '()))                  ; for other modes, do nothing
                )))

    (set! draw-point					; DRAW-POINT
	  (lambda (x y)
	    (%graphics 1 (+ x mid-x) (- mid-y y) cur-color 0 0 0)
	    '()))

    (set! clear-point					; CLEAR-POINT
	  (lambda (x y)
	    (%graphics 1 (+ x mid-x) (- mid-y y) 0 0 0 0)
	    '()))

    (set! is-point-on?					; IS-POINT-ON?
	  (lambda (x y)
	    (positive? (%graphics 4 (+ x mid-x) (- mid-y y) 0 0 0 0))))

    (set! point-color                                   ; POINT-COLOR
          (lambda (x y)
            (%graphics 4 (+ x mid-x) (- mid-y y) 0 0 0 0)))

    (set! position-pen					; POSITION-PEN
	  (lambda (x y)
	    (set! cur-x (+ x mid-x))
            (set! cur-y (- mid-y y))
	    '()))

    (set! get-pen-position                              ; GET-PEN-POSITION
          (lambda ()
            (cons (- cur-x mid-x) (- mid-y cur-y))))

    (set! draw-line-to					; DRAW-LINE-TO
	  (lambda (x y)
	    (let ((old-x cur-x)
		  (old-y cur-y))
	      (position-pen x y)
	      (%graphics 3 old-x old-y cur-x cur-y cur-color 0)
	      '())))

    (set! set-pen-color!				; SET-PEN-COLOR!
	  (lambda (color)
	    (set! cur-color
		  (if (integer? color)
		      (remainder (abs color) num-clrs)
		      (let ((entry (assq color *graphics-colors*)))
			(if entry
			    (remainder (abs (cdr entry)) num-clrs)
			    (-1+ num-clrs)))))))

    (set! get-pen-color                                 ; GET-PEN-COLOR
          (lambda () cur-color))

    (set! set-video-mode!				; SET-VIDEO-MODE!
	  (lambda (mode)
	    (%graphics 0 mode 0 0 0 0 0)
	    (case pcs-machine-type
              (1                        ;TI mode - do nothing special
               '())
              (else                     ;default to IBM
               (case mode
                 (3                     ;IBM CGA
                  (window-set-attribute! pcs-status-window
                                         'text-attributes #x70))
                 ((14 16 18)            ;IBM EGA or VGA
                  (window-set-attribute! pcs-status-window
                                         'text-attributes #x87)))
               (set! cur-w 'screen)
               (if (<> mode 3) 
                   (reset-graphics))))  ;if you're switching modes in IBM,
                                        ;it makes sense to do this too
            '()))

    (set! get-video-mode                                ; GET-VIDEO-MODE
          (lambda ()
            (%graphics 5 0 0 0 0 0 0)))

    (set! draw-box-to					; DRAW-BOX-TO
	  (lambda (x y)
	    (let ((old-x cur-x)
		  (old-y cur-y))
	      (set! cur-x (+ x mid-x))
	      (set! cur-y (- mid-y y))
	      (%graphics 6 old-x old-y cur-x cur-y cur-color 0)
	      '())))

    (set! draw-filled-box-to				; DRAW-FILLED-BOX-TO
	  (lambda (x y)
	    (let ((old-x cur-x)
		  (old-y cur-y))
	      (set! cur-x (+ x mid-x))
	      (set! cur-y (- mid-y y))
	      (%graphics 7 old-x old-y cur-x cur-y cur-color 0)
	      '())))

    (set! set-palette!					; SET-PALETTE!
	  (lambda (arg1 arg2)
	    (when (not (and (integer? arg1)
			    (integer? arg2)))
		  (%error-invalid-operand-list 'SET-PALETTE! arg1 arg2))
	    (when (and (>=? pcs-machine-type #xFC)	; IBM
		       (=? arg1 1)
                       (=? (get-video-mode) 4))
		  (set! *graphics-colors*
			(if (odd? arg2)
			    '((black . 0)(cyan . 1)(magenta . 2)(white . 3))
			    '((black . 0)(green . 1)(red . 2)(yellow . 3)))))
	    (%graphics 2 arg1 arg2 0 0 0 0)
	    '()))

    (set! set-clipping-rectangle!                 ; SET-CLIPPING-RECTANGLE!
          (lambda (x1 y1 x2 y2)   ;left, top, right, bottom
            (%graphics 8 (min max-x (max min-x (+ x1 mid-x)))
                         (min max-y (max min-y (- mid-y y1)))
                         (max min-x (min max-x (+ x2 mid-x)))
                         (max min-y (min max-y (- mid-y y2))) 0 0)
            '()))

    (set! current-graphics-window                 ; CURRENT-GRAPHICS-WINDOW
          (lambda () cur-w))

    (set! graphics-window                         ; GRAPHICS-WINDOW
          (lambda (window)
            (let ((w (if (eq? window 'screen) 'console window)))
              (let ((size (window-get-size w))
                    (pos (window-get-position w))
                    (cbox (cdr (assv (cond ((= pcs-machine-type 1) 'TI)
                                           ((>= pcs-machine-type #xFC) (get-video-mode))
                                           (else pcs-machine-type))
                                     *character-boxes*))))
                (if (null? cbox) (error "Current video mode is not a graphics mode." (get-video-mode)))
                (let* ((left (* (cdr pos) (car cbox)))
                       (top  (* (car pos) (cdr cbox)))
                       (right (sub1 (+ left (* (cdr size) (car cbox)))))
                       (bottom (sub1 (+ top  (* (car size) (cdr cbox))))))
                  (%graphics 8 left top right bottom 0 0)
                  (set! mid-x (quotient (+ left right) 2))
                  (set! mid-y (quotient (+ top bottom) 2))
                  (set! min-x left)
                  (set! min-y top)
                  (set! max-x right)
                  (set! max-y bottom)
                  (set! cur-w window)
                  (list (list (- min-x mid-x) (- mid-y min-y)
                              (- max-x mid-x) (- mid-y max-y))
                        (list left top right bottom)))))))

    '#!false))
