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


;;;; Window Motion Commands

(define next-screen-context-lines 2)

(define-initial-command-key ("^R Next Screen" argument)
  "Move down to display next screenful of text."
(
(define-initial-key  (integer->char 22) procedure)    ;;; C-V
)
  (scroll-window (current-window)
		 (cond ((not argument)
			(- (window-y-size (current-window))
			   next-screen-context-lines))
		       ((command-argument-negative-only?)
			(- next-screen-context-lines
			   (window-y-size (current-window))))
		       (else argument))))

(define-initial-command-key ("^R Previous Screen" argument)
  "Move up to display previous screenful of text."
(
(define-initial-key  (list meta-char #\V) procedure)      ;;; M-V
(define-initial-key (list alt-char (integer->char 47)) procedure)  ;;;alt-v
)
  (scroll-window (current-window)
		 (cond ((not argument)
			(- next-screen-context-lines
			   (window-y-size (current-window))))
		       ((command-argument-negative-only?)
			(- (window-y-size (current-window))
			   next-screen-context-lines))
		       (else (- 0 argument)))))

(define (scroll-window window n)
  (if (if (negative? n)
          (window-mark-visible? window
		 (buffer-start (window-buffer window)))
          (window-mark-visible? window
		 (buffer-end (window-buffer window))))
      (if (negative? n)
	(editor-error "Beginning of buffer")
	(editor-error "End of buffer")))
  (window-scroll-y-relative! window n))


;;;; Kill Commands
;;;; Deletion

(define %delete-check
  (lambda (mark1 mark2)
    (if (not mark2) (editor-error "Delete exceeds buffer bounds"))
    (eq? (mark-line mark1) (mark-line mark2))))

(define-initial-command-key ("^R Backward Delete Character" argument)
  "Delete character before point."
(
(define-initial-key  #\Backspace procedure)
)
  (if (not argument)
      (let ((m1 (mark-1+ (current-point) #!false)))
        (if (%delete-check (current-point) m1)
            (%region-delete-char! m1)
            (delete-region m1)))
      (kill-region (mark- (current-point) argument #!false))))

(define-initial-command-key ("^R Delete Character" argument)
  "Delete character after point."
(
(define-initial-key  (integer->char 4) procedure)     ;;C-D
)
  (if (not argument)
      (let ((m1 (mark1+ (current-point) #!false)))
        (if (%delete-check (current-point) m1)
            (%region-delete-char! (current-point))
            (delete-region m1)))
      (kill-region (mark+ (current-point) argument #!false))))

(define-initial-command-key ("^R Kill Line" argument)
  "Kill to end of line, or kill an end of line."
(
(define-initial-key  (integer->char 11) procedure)     ;;; C-K
)
  (let ((point (current-point)))
    (kill-region
      (cond ((not argument)
	     (let ((end (line-end point 0 #!false)))
	       (if (region-blank? (make-region point end))
		   (mark1+ end #!false)
		   end)))
	    ((positive? argument)
	     (conjunction (not (group-end? point))
			  (line-start point argument 'LIMIT)))
	    ((zero? argument)
	     (line-start point 0 #!false))
	    (else
	     (conjunction (not (group-start? point))
			  (line-start point
				      (if (line-start? point)
					  argument
                                          (1+ argument))
				      'LIMIT)))))))

(define-initial-command-key ("^R Append Next Kill" argument)
  "Make following kill commands append to last batch."
(
(define-initial-key (list meta-char (integer->char 23)) procedure) ;;;M C-W
)
  (set-command-message! append-next-kill-tag))


;;;; Un/Killing

(define-initial-command-key ("^R Kill Region" argument)
  "Kill from point to mark."
(
(define-initial-key (integer->char 23) procedure)       ;;; C-W
)
  (kill-region (current-mark)))

(define-initial-command-key ("^R Copy Region" argument)
  "Stick region into kill-ring without killing it."
(
(define-initial-key (list meta-char #\W) procedure)              ;;; M-W
(define-initial-key (list alt-char (integer->char 17)) procedure);;; alt-W
)
  (copy-region (current-mark)))

(define un-kill-tag
  "Un-kill")

(define-initial-command-key ("^R Un-Kill" (argument 1))
  "Re-insert the last stuff killed."
(
(define-initial-key (integer->char 25) procedure)           ;;; C-Y
)
  (let ((ring (current-kill-ring)))
    (if (or (> argument (ring-size ring))
	    (ring-empty? ring))
	(editor-error "Nothing to un-kill"))
    (if (command-argument-multiplier-only?)
	(un-kill-region (ring-ref ring 0))
	(un-kill-region-reversed (ring-ref ring (-1+ argument)))))
  (set-command-message! un-kill-tag))

(define-initial-command-key ("^R Pop Kill Ring" (argument 1))
  " Pop kill ring"
(
 (define-initial-key (list ctrl-x-char (integer->char 11)) procedure)
)
 (let ((ring (current-kill-ring)))
   (if (> argument (ring-size ring))
       (editor-error "Not enough entries in the kill ring"))
   (ring-stack-pop! ring argument)))

(define-initial-command-key ("^R Un-kill Pop" (argument 1))
  "Correct after ^R Un-Kill to use an earlier kill."
(
(define-initial-key (list meta-char #\Y) procedure)              ;;; M-Y
(define-initial-key (list alt-char (integer->char 21)) procedure);;;Alt-Y
)
  (%edwin-un-kill-pop argument))


;;;; Marks

(define-initial-command-key ("^R Set/Pop Mark" argument)
  "Sets or pops the mark."
(
(define-initial-key (list alt-char (integer->char 3)) procedure)  ;;C-@
)
  (let ((n (command-argument-multiplier-exponent)))
    (cond ((zero? n) (push-current-mark! (current-point))
                     (temporary-message "Mark Set"))
	  ((= n 1) (set-current-point! (pop-current-mark!)))
	  ((= n 2) (pop-current-mark!))
	  (else (editor-error)))))

;;; These are temporarily commented out becuase the C-< and C-> ar blocked
;;; by DSR.

;;;(define-initial-command-key ("^R Mark Beginning" argument)
;;;  "Set mark at beginning of buffer."
;;;(
;;;(define-initial-key (list ctrl-^-char #\<) procedure)      ;;; C-^ <
;;;)
;;;  (push-current-mark! (buffer-start (current-buffer))))
;;;
;;;(define-initial-command-key ("^R Mark End" argument)
;;;  "Set mark at end of buffer."
;;;(
;;;(define-initial-key (list ctrl-^-char #\>) procedure)     ;;; C-^ >
;;;)
;;;  (push-current-mark! (buffer-end (current-buffer))))

(define-initial-command-key ("^R Mark Whole Buffer" argument)
  "Set point at beginning and mark at end of buffer."
(
(define-initial-key (list ctrl-x-char  #\H) procedure)    ;;; C-X H
)
  (push-current-mark! (current-point))
  ((if (not argument) set-current-region! set-current-region-reversed!)
   (buffer-region (current-buffer))))

(define-initial-command-key ("^R Exchange Point and Mark" argument)
  "Exchange positions of point and mark."
(
(define-initial-key (list ctrl-x-char ctrl-x-char) procedure)  ;;; C-X C-X
)
  (let ((point (current-point))
	(mark (current-mark)))
    (if (not mark) (editor-error "No mark to exchange"))
    (set-current-point! mark)
    (set-current-mark! point)))


;;;; Transposition

(define-initial-command-key ("^R Transpose Characters" (argument 1))
  "Transpose the characters before and after the cursor."
(
(define-initial-key (integer->char 20) procedure)  ;;; C-T
)
  (%edwin-transpose-characters argument))



;;; These are commented out becuase are not bound to any keys. These may be
;;; used with extended commands

;;;; Search Commands
;;;; Character Search

;;;(define-initial-command-key ("^R Character Search" argument)
;;;  "Search for a single character."
;;;(#!false)
;;;  (let ((mark
;;;	 (find-next-char (current-point)
;;;			 (buffer-end (current-buffer))
;;;			 (prompt-for-char "Character Search"))))
;;;    (if (not mark) (editor-error))
;;;    (set-current-point! (mark1+ mark #!false))))
;;;
;;;(define-initial-command-key ("^R Reverse Character Search" argument)
;;;  "Search backwards for a single character."
;;;(#!false)
;;;  (let ((mark
;;;	 (find-previous-char (current-point)
;;;			     (buffer-start (current-buffer))
;;;			     (prompt-for-char "Reverse Character Search"))))
;;;    (if (not mark) (editor-error))
;;;    (set-current-point! (mark-1+ mark #!false))))

;;;; String Search

;; **** This is a per-editor variable. ****
   (define previous-successful-search-string "")
;;;
;;;(define-initial-command-key ("^R String Search" argument)
;;;  "Search for a character string."
;;;(#!false)
;;;  (let ((string (prompt-for-string "String Search"
;;;				   previous-successful-search-string)))
;;;    (let ((mark
;;;	   (find-next-string (current-point)
;;;			     (buffer-end (current-buffer))
;;;			     string)))
;;;      (if (not mark) (editor-error))
;;;      (set-current-point! (mark+ mark (string-length string) #!false)))
;;;    (set! previous-successful-search-string string)))
;;;
;;;(define-initial-command-key ("^R Reverse String Search" argument)
;;;  "Search backwards for a character string."
;;;(#!false)
;;;  (let ((string (prompt-for-string "Reverse String Search"
;;;				   previous-successful-search-string)))
;;;    (let ((mark
;;;	   (find-previous-string (current-point)
;;;				 (buffer-start (current-buffer))
;;;				 string)))
;;;      (if (not mark) (editor-error))
;;;      (set-current-point! mark))
;;;    (set! previous-successful-search-string string)))

;;;; Incremental Search

(define-initial-command-key ("^R Incremental Search" argument)
  "Search for character string as you type it."
(
(define-initial-key (integer->char 19) procedure)      ;;; C-S
)
  (incremental-search #!TRUE))

(define-initial-command-key ("^R Reverse Search" argument)
  "Incremental Search Backwards."
(
(define-initial-key (integer->char 18) procedure)     ;;; C-R
)
  (incremental-search #!FALSE))


;;; Word Motion


(define-initial-command-key ("^R Forward Word" (argument 1))
  "Move one or more words forward."
(
(define-initial-key (list meta-char #\f) procedure)               ;;; M-F
(define-initial-key (list alt-char (integer->char 33)) procedure) ;;; alt-F
)
  (move-thing forward-word argument))

(define-initial-command-key ("^R Backward Word" (argument 1))
  "Move one or more words forward."
(
(define-initial-key (list alt-char (integer->char 48)) procedure) ;;; alt-B
(define-initial-key (list meta-char #\b) procedure)               ;;; M-B
)
  (move-thing backward-word argument))

(define-initial-command-key ("^R Mark Word" (argument 1))
  "Set mark one or more words from point."
(
 (define-initial-key (list meta-char #\@) procedure)                ;;; M-@
 (define-initial-key (list alt-char (integer->char 121)) procedure) ;;;alt-@
)
  (mark-thing forward-word argument))

(define-initial-command-key ("^R Kill Word" (argument 1))
 "Kill one or more words forward"
(
 (define-initial-key (list meta-char #\d) procedure)              ;;;M-D
 (define-initial-key (list alt-char (integer->char 32)) procedure);;; Alt D
)
 (kill-thing forward-word argument))

(define-initial-command-key ("^R Backward Kill Word" (argument 1))
  "Kill one or more words backwards"
(
 (define-initial-key (list meta-char #\backspace) procedure) 
)                                                            ;;; alt is blocked
  (kill-thing backward-word argument))



;;; Sentences


(define-initial-command-key ("^R Forward Sentence" (argument 1))
  "Move one or more sentences forward."
(
(define-initial-key (list meta-char #\e) procedure)               ;;; M-E
(define-initial-key (list alt-char (integer->char 18)) procedure) ;;; alt-E
)
  (move-thing forward-sentence argument))

(define-initial-command-key ("^R Backward Sentence" (argument 1))
  "Move one or more sentences forward."
(
(define-initial-key (list alt-char (integer->char 30)) procedure) ;;; alt-A
(define-initial-key (list meta-char #\a) procedure)               ;;; M-A
)
  (move-thing backward-sentence argument))


(define-initial-command-key ("^R Kill Sentence" (argument 1))
 "Kill one or more sentences forward"
(
 (define-initial-key (list meta-char #\k) procedure)              ;;;M-K
 (define-initial-key (list alt-char (integer->char 37)) procedure);;; Alt K
)
 (kill-thing forward-sentence argument))

(define-initial-command-key ("^R Backward Kill Sentence" (argument 1))
  "Kill one or more sentences backwards"
(
 (define-initial-key (list ctrl-x-char #\backspace) procedure) 
)                                     
  (kill-thing backward-sentence argument))



(define-initial-command-key ("^R Forward Paragraph" (argument 1))
  "Move one or more paragraph forward."
(
(define-initial-key (list meta-char #\]) procedure)               ;;; M-]
)
  (move-thing forward-paragraph argument))

(define-initial-command-key ("^R Backward Paragraph" (argument 1))
  "Move one or more sentences forward."
(
(define-initial-key (list meta-char #\[) procedure)               ;;; M-[
)
  (move-thing backward-paragraph argument))


(define-initial-command-key ("^R Mark Paragraph" (argument 1))
  "mark the beginning and end of the paragraph"
(
 (define-initial-key (list meta-char #\h) procedure)
 (define-initial-key (list alt-char (integer->char 35)) procedure)
)
  (let ((end (forward-paragraph (current-point) 1 'ERROR)))
    (set-current-region! (make-region (backward-paragraph end 1 'ERROR) end))))




