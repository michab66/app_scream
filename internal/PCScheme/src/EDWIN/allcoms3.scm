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

;;;Lisp commands

(define *current-mode-scheme?* #!true)

(define ^r-lisp-insert-paren-command '())			;3.02
(define paren-mark '())						;3.02
(define (cached-paren-mark) paren-mark)				;3.02
(define (cache-paren-mark mark) (set! paren-mark mark))		;3.02

(define-initial-command-key ("^R Lisp Insert Paren" (argument 1))
  "Insert close paren, showing matching parens"
(              ;;;;(
 (define-initial-key  #\) procedure)
 (set! ^r-lisp-insert-paren-command procedure)			;3.02
)
  (insert-chars (current-command-char) argument (current-point))
  (if *current-mode-scheme?*
      (if (not (char-ready? buffer-screen))
          (let ((mark (if (cached-paren-mark)			;3.02
                          (backward-sexp:top (cached-paren-mark)  ;3.02
                                             (group-start (current-point))
                                             1)
                          (backward-one-list (current-point)
                                             (group-start (current-point))))))
            (if mark
                (let ((string (line-string (mark-line mark))))
                  (cache-paren-mark mark)			;3.02
                  (set-temp-message-status)
                  (set-screen-cursor! typein-screen 0 0)
                  (%substring-display string (mark-position mark) 
				      (string-length string) 0 typein-screen)
                  (if (window-mark-visible? (current-window) mark)
                      (let ((old-point (current-point)))
                        (set-current-point! mark)
                        (with-reverse-attributes)
                        (set-current-point! old-point))))
		(beep))))))

;;;(define %%temp (lambda () (with-reverse-attributes)))

;;;


;;;

(define-initial-command-key ("^R Forward Sexp" (argument 1))
"Move forward one sexp"
(
(define-initial-key (list meta-char (integer->char 6)) procedure) ;;; M C-F
)
(move-thing forward-sexp argument))

(define-initial-command-key ("^R Backward Sexp" (argument 1))
"Move backward one sexp"
(
(define-initial-key (list meta-char (integer->char 2)) procedure) ;;; M C-B
)
(move-thing backward-sexp argument))

(define-initial-command-key ("^R Mark Sexp" (argument 1))
  "Set mark one or more sexp from point."
(
 (define-initial-key (list meta-char alt-char (integer->char 3)) procedure)
                                                            ;;; C-M-@
)
  (mark-thing forward-sexp argument))

(define-initial-command-key ("^R Kill Sexp" (argument 1))
 "Kill one or more sexp forward"
(
 (define-initial-key (list meta-char (integer->char 11)) procedure) ;;; M C-K
)
 (kill-thing forward-sexp argument))

;;;(define-initial-command-key ("^R Backward Kill sexp" (argument 1))
;;;  "Kill one or more words backwards"
;;;(
;;; (define-initial-key (list ctrl-z-char #\backspace) procedure)   ;;; C-Z backsp
;;;)
;;;  (kill-thing backward-sexp argument))

(define-initial-command-key ("^R Forward List"(argument 1))
  "Move forward over one list"
(
(define-initial-key (list meta-char (integer->char 14)) procedure) ;; M C-N
)
  (move-thing forward-list argument))

(define-initial-command-key ("^R Backward List"(argument 1))
  "Move backward over one list"
(
(define-initial-key (list meta-char (integer->char 16)) procedure) ;; M C-P
)
  (move-thing backward-list argument))

(define-initial-command-key ("^R Forward Down List" (argument 1))
  "Move down one level of list structure, forward."
(
(define-initial-key (list meta-char (integer->char 4)) procedure) ;;M C-D
)
  (move-thing forward-down-list argument))

;;; (define-initial-command-key ("^R Backward Down List" (argument 1))
;;;  "Move down one level of list structure, backward."
;;;(#!false)
;;;  (move-thing backward-down-list argument))

;;;(define-initial-command-key ("^R Forward Up List" (argument 1))
;;;  "Move up one level of list structure, forward."
;;;(                                                      ;;;(
;;; (define-initial-key (list ctrl-z-char #\) ) procedure)    ;;; ( C-Z )
;;;)
;;;  (move-thing forward-up-list argument))

(define-initial-command-key ("^R Backward Up List" (argument 1))
  "Move up one level of list structure, backward."
(
(define-initial-key (list meta-char (integer->char 21)) procedure)
)
  (move-thing backward-up-list argument))


;;; New commands added

;;; Some additional commands

;;; File commands

(define-initial-command-key ("^R Set File Read Only" argument)
  " Make file read-only, or not."
(
 (define-initial-key (list ctrl-x-char (integer->char 17)) procedure);;C-XC-Q
)
  (setup-current-buffer-read-only! argument))


(define-initial-command-key ("^R Buffer Not Modified" argument)
  "Pretend that buffer has not been Modified."
(
 (define-initial-key (list meta-char #\~) procedure) ;; M-~
)
 (buffer-not-modified! (current-buffer)))


;;; Line Commands

(define-initial-command-key ("^R Open Line" (argument 1))
  "Insert a newline at point. Cursor remains at its position."
(
 (define-initial-key (integer->char 15) procedure)     ;;;; C-O
)
 (let ((m* (mark-right-inserting (current-point))))
   (insert-newlines argument )
   (set-current-point! m*)))

(define-initial-command-key ("^R Set Goal Column" argument)
  "Set (or flush) a permanent goal for vertical motion"
(
 (define-initial-key (list ctrl-x-char (integer->char 14)) procedure)
)                                                  ;;; C-X C-N
 (set! goal-column
       (and (not argument)
            (mark-column (current-point)))))

(define-initial-command-key ("^R Tab" (argument 1))
  "Insert a tab character"
(
 (define-initial-key #\tab procedure)
 (define-initial-key (integer->char 9) procedure)
 (define-initial-key (list meta-char #\tab) procedure)
)
 (if *current-mode-scheme?* 
     (lisp-indent-line (current-point))
     (insert-chars #\tab argument (current-point))))


(define-initial-command-key ("^R Indent Sexp" (argument 1))
  "Indent a sexp"
(
 (define-initial-key (list meta-char (integer->char 17)) procedure) ;;M C-Q
)
 (if *current-mode-scheme?* 
     (lisp-indent-sexp (current-point))))

(define-initial-command-key ("^R Change Mode" argument)
" Change mode to Scheme"
(
 (define-initial-key (list ctrl-x-char (integer->char 13)) procedure);;C-X C-M
)
 (set! *current-mode-scheme?* (if *current-mode-scheme?* #!false #!true))
 (window-modeline-event! '() 'mode-changed))




(define-initial-command-key ("^R Delete Horizontal Space" argument)
  " delete all spaces and tab characters around point."
(
  (define-initial-key (list meta-char #\\) procedure)    ;;; M-\
)
  (delete-horizontal-space))

(define-initial-command-key ("^R Just One Space" argument)
  " Delete all spaces and tabs around point, leaving one Space."
(
  (define-initial-key (list meta-char #\space) procedure) ;;; M-space
)
  (delete-horizontal-space)
  (insert-chars #\space 1 (current-point)))

(define lisp-indent 2)

(define-initial-command-key ("^R Indent New Line" argument)
  "Insert new line then indent the second line"
(
  (define-initial-key (integer->char 10) procedure) ;;; C-J
)
  (insert-newlines 1)
  (if *current-mode-scheme?*
      (lisp-indent-line (current-point))
      (insert-chars #\tab 1 (current-point))))


;;; compile command

(define-initial-command-key ("^R Compile Region" argument)
  " Compile the region"
 (
  (define-initial-key (list meta-char (integer->char 26)) procedure);;M C-Z
 )
  (if *current-mode-scheme?* 
      (%compile-region
         (make-region (current-point) (current-mark)))
      (^r-bad-command argument)))

(define-initial-command-key ("^R Compile Buffer" argument)
  " Compile the buffer"
 (
  (define-initial-key (list meta-char #\o) procedure)   ;;; M-O
  (define-initial-key (list alt-char (integer->char 24)) procedure) ;;;alt O
 )
  (if *current-mode-scheme?* 
      (%compile-region
         (buffer-region (current-buffer)))
      (^r-bad-command argument)))

(define-initial-command-key ("^R Compile Sexp" (argument 1))
  " Compile the sexp"
 (
  (define-initial-key (list meta-char (integer->char 24)) procedure);;;M C-X
 )
  (if *current-mode-scheme?* 
      (begin
       (mark-thing forward-sexp argument)
       (%compile-region (current-region)))
      (^r-bad-command argument)))

(define (%compile-region region)
  (region->file region "edwin.tmp")
  (restore-console-contents)
  (make-pcs-status-visible)
  (reset-typein-window)
  (gc)
  (load "edwin.tmp")
  ((fluid editor-continuation) 'OK))

(define-initial-command-key ("^R Toggle windows" argument)
  " Display edwin window in upper half and scheme in the lower half"
 (
  (define-initial-key (list ctrl-x-char #\!) procedure)   ;;; C-X !
 )
  (if *split-screen-mode?*
      (begin
       (set! *split-screen-mode?* #!false)
       (move-editor-to-full)
       (move-pcs-to-full)
       (make-pcs-status-invisible)
       (window-y-size-changed (current-window))
       (update-display! (current-window))
       (reset-modeline-window)
       (reset-typein-window))
      (begin
       (set! *split-screen-mode?* #!true)
       (move-editor-to-upper-half)
       (move-pcs-window-lower)
       (window-y-size-changed (current-window))
       (update-display! (current-window))
       (reset-modeline-window)
       (reset-typein-window)
       (restore-console-contents)
       (make-pcs-status-visible)
       (gc))))



(define edwin-reset-windows
  (lambda ()
    (save-console-contents)
    (make-pcs-status-visible)
    (move-pcs-to-full)
    (%clear-window blank-screen)
    (restore-console-contents)
    (gc)))


