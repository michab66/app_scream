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

;;; modeline messages
;012345678901234567890123456789012345678901234567890123456789012345678901234567
;PCS Edwin VVVVVVVVVV Filename for the rest of the line
; cols 78 and 79 are reserved for the modified stars
(begin
(define-integrable name-position 4)
(define-integrable version-position 11)
(define-integrable version-length 6)
(define-integrable mode-position 19)
(define-integrable file-name-position 35)
(define-integrable file-name-length 31)
(define-integrable modified-position 0)
(define-integrable buffer-position 17)
)

(define reset-modeline-window #!false)
(define window-modeline-event! #!false)
(define update-modeline! #!false)

(letrec
 ((file-name #!false)
  (file-name-changed #!false)
  (version Edwin-Version)
  (modified #!false)
  (modified-changed #!false)
  (mode-changed #!false)
  (mode-scheme? #!true)
  (position-cursor
   (lambda (pos)
     (%reify-port! modeline-screen screen:cursor-x pos)))
  (string-upcase
   (lambda (string)
     (and string
     (let loop ((string1 (make-string (string-length string) #\space))
		(index 0) (end (string-length string)))
	  (if (< index end)
	      (begin
               (string-set! string1 index
                            (char-upcase (string-ref string index)))
               (loop string1 (1+ index) end))
              string1)))))
  (write-modified (lambda ()
		    (set! modified-changed #!false)
		    (position-cursor modified-position)
		    (princ (if modified "**" "  ") modeline-screen)))
  (write-mode (lambda ()
		(set! mode-changed #!false)
		(position-cursor mode-position)
		(princ (if mode-scheme? "   [Scheme]    "
			   " [Fundamental] ")
		       modeline-screen)))
  (write-file-name (lambda ()
		     (set! file-name-changed #!false)
		     (clear-subscreen! modeline-screen
				       file-name-position 0 0
				       file-name-length)
		     (position-cursor file-name-position)
		     (if file-name (princ file-name modeline-screen)))))

(set! reset-modeline-window
  (lambda ()
    (let ((buffer (current-buffer)))
      (set! modified (buffer-modified? buffer))
      (set! modified-changed #!true)
      (set! file-name (string-upcase (buffer-pathname buffer)))
      (set! file-name-changed #!true)
      (set! mode-scheme? *current-mode-scheme?*)
      (set! mode-changed #!true)
      (%clear-window modeline-screen)
      (%reify-port! modeline-screen screen:cursor-y 0)
      (position-cursor name-position)
      (princ "Edwin" modeline-screen)
      (position-cursor version-position)
      (princ version modeline-screen)
;;;      (position-cursor buffer-position)
;;;      (princ " Buffer : Main " modeline-screen)
      (update-modeline!))))

(set! window-modeline-event!
  (lambda (window event)
    (let ((buffer (current-buffer)))
      (cond ((eq? event 'buffer-modified)
	     (let ((buffer-modified (buffer-modified? buffer)))
               (if (not (eq? buffer-modified modified))
		   (set! modified-changed #!true))
	       (set! modified buffer-modified)))
	    ((eq? event 'buffer-pathname)
	     (set! file-name-changed #!true)
	     (set! file-name (string-upcase (buffer-pathname buffer))))
	    ((eq? event 'mode-changed)
	     (set! mode-scheme? *current-mode-scheme?*)
	     (set! mode-changed #!true))
	    (else #!false)))))


(set! update-modeline!
  (lambda ()
    (if modified-changed (write-modified))
    (if file-name-changed (write-file-name))
    (if mode-changed (write-mode)))))






