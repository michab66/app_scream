; Window and attribute functions for PC Scheme
; Copyright 1987,1988 (c) Texas Instruments


;; NEW-WINDOW - new version for 3.02

; NEW-WINDOW creates a window interactively.  The cursor can be moved
; around to mark the upper left hand and lower right hand corners of the
; window.  The window port object is returned.
;
; This function demonstrates how to create a non-destructive cursor
; in PC Scheme by using a popup window of size 1x1.
;
; Example: (new-window "A Window") -> port object

;; Create a new window using the cursor keys and return the port object.
;; The cursor keys position the corner markers, return accepts the
;; marker's position, and any other key exits with no change.
;; "minrows" and "mincols" say that the window will be at least that big.
;; The window is displayed immediately unless the symbol NO-DISPLAY is used.
;; The new window always has a border.
;; syntax:  (NEW-WINDOW title [minrows] [mincols] ['NO-DISPLAY])
(define (new-window title . rest)
  (let ((minrows (or (car rest) 0))
        (mincols (or (cadr rest) 0))
        (no-display (memq 'no-display rest)))
    (call/cc
      (lambda (exit)
        (letrec ((ulc (integer->char 218))
                 (rlc (integer->char 217))
                 (left #\K)
                 (up #\H)
                 (right #\M)
                 (down #\P)
                 (accept #\return) 
                 (hold '())
                 (cursor 
                   (let ((w (make-window "" #!false)))
                     (window-set-size! w 1 1)
                     w))
                 (read-char-1
                   (lambda ()
                     (let ((char (read-char cursor)))
                       (if (char=? char (integer->char 0)) 
                           (read-char cursor) char))))
                 (mark-corner
                   (lambda (uly ulx lry lrx ch)   ;note y,x means row,col
                     (let loop ((r uly)
                                (c ulx))
                       (window-set-position! cursor r c)
                       (window-popup cursor)
                       (display ch cursor)
                       (window-set-cursor! cursor 0 0)
                       (let ((char (read-char-1)))
                         (window-popup-delete cursor)
                         (cond ((eqv? char left)  
                                (loop r (if (>= (-1+ c) ulx) (-1+ c) c)))
                               ((eqv? char up)
                                (loop (if (>= (-1+ r) uly) (-1+ r) r) c))
                               ((eqv? char right) 
                                (loop r (if (< (1+ c) lrx) (1+ c) c)))
                               ((eqv? char down) 
                                (loop (if (< (1+ r) lry) (1+ r) r) c))
                               ((eqv? char accept) 
                                (window-set-cursor! cursor 0 0)
                                (set! hold
                                      (list (window-save-contents cursor) r c))
                                (display ch cursor)
                                (cons r c))
                               (else
                                (and hold 
                                     (let ((char (car hold))
                                           (r (cadr hold))
                                           (c (caddr hold)))
                                       (window-set-position! cursor r c)
                                       (window-restore-contents cursor char)))
                                (exit #!false))))))))
          (let* ((uly (car (window-get-position (current-output-port))))
                 (ulx (cdr (window-get-position (current-output-port))))
                 (lry (+ uly (car (window-get-size (current-output-port)))))
                 (lrx (+ ulx (cdr (window-get-size (current-output-port)))))
                 (ulc-position (mark-corner uly ulx 
                                            (- lry minrows) (- lrx mincols)
                                            ulc))
                 (new-uly (car ulc-position))
                 (new-ulx (cdr ulc-position))
                 (rlc-position (mark-corner (+ new-uly minrows) 
                                            (+ new-ulx mincols) lry lrx rlc))
                 (new-lry (car rlc-position))
                 (new-lrx (cdr rlc-position))
                 (new-width (1+ (- new-lrx new-ulx)))
                 (new-height (1+ (- new-lry new-uly)))
                 (w (make-window title t)))
            (window-set-position! w new-uly new-ulx)
            (window-set-size! w new-height new-width)
            (or no-display (window-clear w))
            w))))))



; ATTR takes a list of attribute names and converts them to the
; equivalent attribute number suitable for PC Scheme's attribute
; functions.  It works with both TI and IBM (CGA only).
;
; Examples: (attr)                 ;returns default value
;           (attr '(red blink))    ;returns number for blinking red text;
;                                  ;exact number depends on the machine type
;           (attr 'ti  '(red blink)) ;ignore machine type, get attr# for TI
;           (attr 'ibm '(red blink)) ;ignore machine type, get attr# for IBM
;
(define ATTR
  (let ((attrs-ibm '((blink . 128) (bkg-white . 112)
                     (bkg-brown . 96) (bkg-magenta . 80) (bkg-cyan . 48)
                     (bkg-red . 64) (bkg-green . 32) (bkg-blue . 16)
                     (light-white . 15) (yellow . 14)
                     (light-magenta . 13) (light-red . 12)
                     (light-cyan . 11) (light-green . 10) (light-blue . 9)
                     (gray . 8) (white . 7) (brown . 6) (magenta . 5)
                     (red . 4) (cyan . 3) (green . 2) (blue . 1) (BLACK . 0)))
        (attrs-ti  '((ALTCHAR . 128) (BLINK . 64)
                     (UNDERLINE . 32) (REVERSE . 16) (NODSP . -8)
                     (WHITE . 7) (YELLOW . 6) (cyan . 5) (GREEN . 4)
                     (PURPLE . 3) (RED . 2) (blue . 1) (BLACK . 0)))
        (default-attrs-ibm 15)
        (default-attrs-ti 15)
        (prime-ibm 0)
        (prime-ti 8))
    (lambda x
      (let ((work-fn
              (LAMBDA (attrs default acc)
                (COND
                  ((NULL? X)
                   (SET! ACC default))
                  ((NUMBER? (CAR X))
                   (SET! ACC (CAR X)))
                  (else
                    (MAPC
                      (LAMBDA (x)
                        (let ((attr-value (assq x attrs)))
                          (and attr-value
                               (set! acc (+ acc (cdr attr-value))))))
                      x)))
                (and (=? pcs-machine-type 1)   ;keep text enabled in TI mode
                     (=? acc prime-ti)
                     (set! acc default))
                acc)))
        (case (car x)
          (ti
            (set! x (cdr x))
            (work-fn attrs-ti  default-attrs-ti  prime-ti))
          (ibm
            (set! x (cdr x))
            (work-fn attrs-ibm default-attrs-ibm prime-ibm))
          (else
            (if (=? pcs-machine-type 1)
                (work-fn attrs-ti  default-attrs-ti  prime-ti)
                (work-fn attrs-ibm default-attrs-ibm prime-ibm))))))))
