
;      -*- Mode: Lisp -*-			     Filename:  pwindows.s

;                     Last Revision:  10-Oct-85 1500ct

;--------------------------------------------------------------------------;
;									   ;
;                         TI SCHEME -- PCS Compiler                        ;
;                  Copyright 1985 (c) Texas Instruments                    ;
;									   ;
;                               John Jensen                                ;
;									   ;
;                      Window Manipulation Routines                        ;
;									   ;
;--------------------------------------------------------------------------;


;;;   MAKE-WINDOW returns a "default" window object with the following
;;;   attributes:
;;;
;;;	   Upper Left Hand Corner     = 0,0
;;;	   Size (Lines, Columns)      = 25,80 or 30,80 (the entire screen)
;;;	   Cursor Position	      = 0,0
;;;	   Text Color	              = White (on IBM, high intensity white)
;;;	   Border Color (if bordered) = Green (on IBM, low intensity green)
;;;	   Transcript Recording       = Enabled

(define make-window					; MAKE-WINDOW
  (lambda args
    (let ((label (car args))
	  (bordered? (cadr args)))
      (if (or (null? label) (string? label))
	  (let ((window (%make-window label)))
	    (when bordered?
		  (%reify-port! window 6 (if (eqv? pcs-machine-type 1)
					     #b00001100		; TIPC green
					     #b00001010)))	; IBM green
	    window)
	  (begin
	    (%error-invalid-operand 'MAKE-WINDOW label)
	    '())))))


;;;   WINDOW-CLEAR erases the data portion of a window (writes blanks using
;;;	the current text attributes) and positions the cursor in position
;;;	0,0 (the upper left hand corner of the window).  If the window is
;;;	bordered, the border is re-drawn by this operation.  This operation
;;;	more properly may be considered a "window-initialize" operation.

(define WINDOW-CLEAR					; WINDOW-CLEAR
  (lambda (window)
    (if (or (window? window) (null? window))
        (%clear-window window)
	(begin
	  (%error-invalid-operand 'WINDOW-CLEAR window)
	  '()))))


;;;   The "delete-window" function completely erases the area of the CRT which
;;;	is covered by a given window, including the borders.  This function
;;;	accomplishes the erasing of the borders by expanding the dimensions
;;;	of the window (temporarily) so that the borders are included in the
;;;	data portion of the window; setting the border attribute to "no
;;;	border"; and issuing a "%clear-window" operation to clear the text
;;;	portion of the (temporarily) expanded window.  After clearing the
;;;	window and border, the original attributes of the window are
;;;	restored.
;;;
;;;	Note:  when expanding the size of the window to account for the
;;;	right and bottom borders, this routine takes advantage of the fact
;;;	that %reify-port will not allow a window's boundaries to be set
;;;	to be larger than the physical device size.  Therefore, no check
;;;	is performed to see if the right and bottom borders are off the
;;;	screen.

(define WINDOW-DELETE					; DELETE-WINDOW
  (lambda (window)
    (if (or (window? window) (null? window))
      (if (eqv? (%reify-port window 6) -1)
	  (%clear-window window) ; if not bordered, just do a %clear-window
	  (let ((ul-line (%reify-port window 2)) ; save current attributes
		(ul-col  (%reify-port window 3)) ;  for later restoration
		(n-lines (%reify-port window 4))
		(n-cols  (%reify-port window 5))
		(b-attrib (%reify-port window 6))
		(t-lines '())
		(t-cols '()))
	    (begin
	      (when (> ul-line 0)
		    (begin ; increase window size to include top border
		       (%reify-port! window 2 (-1+ ul-line))
		       (%reify-port! window 4 (1+ n-lines))))
	      (when (> ul-col 0)
		    (begin ; increase window size to include left border
		      (%reify-port! window 3 (-1+ ul-col))
		      (%reify-port! window 5 (1+ n-cols))))
	      (set! t-lines (%reify-port window 4)) ; get new window size
	      (set! t-cols (%reify-port window 5))
	      (%reify-port! window 4 (1+ t-lines)) ; include bottom border
	      (%reify-port! window 5 (1+ t-cols)) ; include right border
	      (%reify-port! window 6 -1)	; indicate no border
	      (%clear-window window)
	      (%reify-port! window 2 ul-line) ; restore the original
	      (%reify-port! window 3 ul-col)  ;  attributes to the user's
	      (%reify-port! window 4 n-lines) ;  window
	      (%reify-port! window 5 n-cols)
	      (%reify-port! window 6 b-attrib))))
      (begin
        (%error-invalid-operand 'WINDOW-DELETE window)
	'()))))


;;;   WINDOW-GET-POSITION conses the coordinates of the upper left hand
;;;	position of a window into a pair as:  (line . column)

(define WINDOW-GET-POSITION				; WINDOW-GET-POSITION
  (lambda (window)
    (if (or (window? window) (null? window))
	(cons (%reify-port window 2) (%reify-port window 3))
	(begin
	  (%error-invalid-operand 'WINDOW-GET-POSITION window)
	  '()))))


;;;   WINDOW-GET-SIZE conses the number of lines and columns in a window
;;;	(excluding the border columns, if any) into a pair as:
;;;	(lines . columns)

(define WINDOW-GET-SIZE					; WINDOW-GET-SIZE
  (lambda (window)
    (if (or (window? window) (null? window))
	(cons (%reify-port window 4) (%reify-port window 5))
	(begin
	  (%error-invalid-operand 'WINDOW-GET-SIZE window)
	  '()))))


;;;   WINDOW-GET-CURSOR conses the line and column number of the current
;;; 	cursor position into a pair as:  (line . column)

(define WINDOW-GET-CURSOR				; WINDOW-GET-CURSOR
  (lambda (window)
    (if (or (window? window) (null? window))
	(cons (%reify-port window 0) (%reify-port window 1))
	(begin
	  (%error-invalid-operand 'WINDOW-GET-CURSOR window)
	  '()))))


;;;   The following routines modify the position, size, and cursor position
;;;	of a window by side effecting the appropriate fields in a window
;;;	object.  An argument value of '() indicates that a particular
;;;	field's value is to remain unchanged.

(define WINDOW-SET-POSITION!)
(define WINDOW-SET-SIZE!)
(define WINDOW-SET-CURSOR!)
(letrec ((chk-and-set
	  (lambda (window line column instruction-name L C)
	    (cond
	     ((not (or (window? window) (null? window)))
	      (error (string-append "Invalid Window Argument to "
				    (symbol->string instruction-name))
		     window))
	     ((and line
		   (or (not (integer? line))
		       (negative? line)))
	      (error (string-append "Invalid Line Number to "
				    (symbol->string instruction-name))
		     line))
	     ((and column
		   (or (not (integer? column))
		       (negative? column)))
	      (error (string-append "Invalid Column Number to "
				    (symbol->string instruction-name))
		     column))
	     (else
	      (when line (%reify-port! window L line))
	      (when column (%reify-port! window C column))
	      window)))))
   (set! WINDOW-SET-POSITION!				; WINDOW-SET-POSITION!
	 (lambda (window ul-line ul-col)
	   (chk-and-set window ul-line ul-col
			'WINDOW-SET-POSITION! 2 3)))
   (set! WINDOW-SET-SIZE!				; WINDOW-SET-SIZE!
	 (lambda (window n-lines n-cols)
	   (chk-and-set window n-lines n-cols
			'WINDOW-SET-SIZE! 4 5)))
   (set! WINDOW-SET-CURSOR!				; WINDOW-SET-CURSOR!
	 (lambda (window cur-line cur-col)
	   (chk-and-set window cur-line cur-col
			'WINDOW-SET-CURSOR! 0 1))))


;;;     Pop-Up window manipulation.
;;;
;;;     "WINDOW-POPUP" preserves the data on the screen which will be
;;;	covered by the pop-up window, initializes the window, and
;;;	returns the pop-up window object to the caller.
;;;
;;;     "WINDOW-POPUP-DELETE" restores the region of the CRT covered by a
;;;	window created "WINDOW-POPUP" to its state prior to the
;;;	pop-up window's appearance.

(define WINDOW-POPUP)
(define WINDOW-POPUP-DELETE)
(let ((pop-up-list '()))
  (begin
    (set! WINDOW-POPUP					; WINDOW-POPUP
      (lambda (window)
        (if (or (window? window) (null? window))
	  (begin
	    (set! pop-up-list
	      (cons (cons window (window-save-contents window)) pop-up-list))
	    (window-delete window)
	    (%clear-window window)
	    window)
	  (begin
	    (%error-invalid-operand 'WINDOW-POPUP window)
	    '()))))
    (set! WINDOW-POPUP-DELETE				; WINDOW-POPUP-DELETE
      (lambda (window)
	(let ((saved-data (assq window pop-up-list)))
	  (when (not (null? saved-data))
		(window-restore-contents window (cdr saved-data))
		(set! pop-up-list (delq! saved-data pop-up-list))
		window)))) ))


;;;   The following routines get and set window attributes which are not
;;;	modifiable by any of the above routines.  It is necessary to explicitly
;;;	name the attribute you wish to examine/modify.

(define WINDOW-GET-ATTRIBUTE)
(define WINDOW-SET-ATTRIBUTE!)
(letrec ((attr-list '((border-attributes . 6)
		     (text-attributes . 7)
		     (window-flags . 8)))
	 (check-and-map-args
	   (lambda (window attribute)
	     (if (or (window? window) (null? window))
	       (cdr (assq attribute attr-list))
	       #!FALSE))))
  (set! WINDOW-GET-ATTRIBUTE
    (lambda (window attribute)
      (let ((mapped-attribute (check-and-map-args window attribute)))
	(if mapped-attribute
	    (%reify-port window mapped-attribute)
	    (begin
	      (%error-invalid-operand-list 'WINDOW-GET-ATTRIBUTE
					   window attribute)
	      '())))))
  (set! WINDOW-SET-ATTRIBUTE!
    (lambda (window attribute value)
      (let ((mapped-attribute (check-and-map-args window attribute)))
	(if (and mapped-attribute
		 (integer? value)
		 (< value #x3fff)
		 (> value #x-3fff))
	    (%reify-port! window mapped-attribute value)
	    (begin
	      (%error-invalid-operand-list 'WINDOW-SET-ATTRIBUTE!
					   window attribute value)
	      '()))))))
