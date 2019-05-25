;;;
;;;     Copyright (c) 1985 Massachusetts Institute of Technology
;;;
;;;     This material was developed by the Scheme project at the
;;;     Massachusetts Institute of Technology, Department of
;;;     Electrical Engineering and Computer Science.  Permission to
;;;     copy this software, to redistribute it, and to use it for any
;;;     purpose is granted, subject to the following restrictions and
;;;     understandings.
;;;
;;;     1. Any copy made of this software must include this copyright
;;;     notice in full.
;;;
;;;     2. Users of this software agree to make their best efforts (a)
;;;     to return to the MIT Scheme project any improvements or
;;;     extensions that they make, so that these may be included in
;;;     future releases; and (b) to inform MIT of noteworthy uses of
;;;     this software.
;;;
;;;     3.  All materials developed as a consequence of the use of
;;;     this software shall duly acknowledge such use, in accordance
;;;     with the usual standards of acknowledging credit in academic
;;;     research.
;;;
;;;     4. MIT has made no warrantee or representation that the
;;;     operation of this software will be error-free, and MIT is
;;;     under no obligation to provide any services, by way of
;;;     maintenance, update, or otherwise.
;;;
;;;     5.  In conjunction with products arising from the use of this
;;;     material, there shall be no use of the name of the
;;;     Massachusetts Institute of Technology nor of any adaptation
;;;     thereof in any advertising, promotional, or sales literature
;;;     without prior written consent from MIT in each case.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;     Modified by Texas Instruments Inc 8/15/85
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; This has been done to do most of the stuff at compile time rather
;;; than at load time. The commands and key definition are combined into
;;; one.
;;;
;;; The default key needs to be the first thing defined.
;;; With the current state of edwin (with the absence of extended
;;; commands) we do not need the following files comman.scm strtab.scm
;;; nvector.scm. Some of these may be needed with extended commands.
;;; All the initial commands assume that they are first ones of their
;;; name being defined. No checks are made

;;; instead of flooding the name space with all possible commands
;;; we define only those which are explicitly needed.

(define ^r-insert-self-command '())
(define ^r-argument-digit-command '())
(define ^r-forward-character-command '())
(define ^r-backward-character-command '())
(define ^r-negative-argument-command '())
(define ^r-bad-command '())
;;;

(define alt-char (integer->char 0))
(define meta-char (integer->char 27))
(define ctrl-x-char (integer->char 24))
(define ctrl-z-char (integer->char 26))

;;;

(define *split-screen-mode?* #!false)

;;;

;;;; Basic Commands

(define-initial-command-key ("^R Bad Command" argument)
  "This command is used to capture undefined keys."
(
(define-initial-default-key  procedure)
(set! ^r-bad-command procedure)
)
  (editor-error (string-append "Undefined command: "
                                (obj->string (current-command-char)))))

(define-initial-command-key ("^R Insert Self" (argument 1))
  "Insert the character used to invoke this."
(
(define add-insert-self
  (lambda (lower upper)
    ((rec loop
       (lambda (n)
         (if (> n upper)
             #!false
             (begin
               (define-initial-key (integer->char n) procedure)
               (loop (1+ n))))))
     lower)))

(add-insert-self 32 40)
(add-insert-self 42 47)
(add-insert-self 58 64)
(add-insert-self 91 127)
(add-insert-self 128 254)         ;;; add new code for internationalize
(set! ^r-insert-self-command procedure)
)
  (insert-chars (current-command-char) argument (current-point)))

(define-initial-command-key ("^R Quoted Insert" (argument 1))
  "Insert the next character typed"
  ((define-initial-key (integer->char 17) procedure))        ;;; C-Q
  (insert-chars (editor-read-char buffer-screen) argument (current-point)))

(define (insert-newlines n)
  (let ((point (current-point)))
    (cond ((= n 1) (region-insert-newline! point))
        ((> n 1) (region-insert-string! point (make-string n #\Newline))))))

(define (insert-chars char n point)
  (cond ((= n 1) (region-insert-char! point char))
        ((> n 1) (region-insert-string! point (make-string n char)))))


(define execute-extended-chars?
  #!TRUE)

(define (set-command-prompt-prefix! prefix-string)
  (set-command-prompt!
   (string-append-with-blanks (command-argument-prompt)
                            prefix-string)))

(define-initial-command-key ("^R Prefix Character" argument)
  "This is a prefix for more commands."
(
(define-initial-prefix-key  meta-char procedure)
(define-initial-prefix-key alt-char procedure)
(define-initial-prefix-key  ctrl-x-char procedure)
(define-initial-prefix-key (list meta-char alt-char) procedure)
)
  (let ((prefix-char (current-command-char)))
    (set-command-prompt-prefix!
         (string-append (char->name prefix-char) " "))
    (let ((char (editor-read-char (window-screen (current-window)))))
         (dispatch-on-char (if (atom? prefix-char)
                               (list prefix-char char)
                               (append prefix-char (list char)))))))

(define-initial-command-key ("^R Meta Character" argument)
  "This is a prefix for more commands."
(
(define-initial-prefix-key  ctrl-z-char procedure)
)
  (let ((prefix-char meta-char))
    (set-command-prompt-prefix!
         (string-append (char->name prefix-char) " "))
    (let ((char (editor-read-char (window-screen (current-window)))))
         (dispatch-on-char (list prefix-char char)))))


(define-initial-command-key ("^R Scheme" argument)
  "Stop Edwin and return to Scheme."
(
(define-initial-key  (list ctrl-x-char (integer->char 26)) procedure);;;C-X C-Z
)
  (save-buffer-changes (current-buffer))
  (edwin-exit))

(define-initial-command-key ("^R Exit" argument)
  "Stop Edwin, remove internal data structures, and return to scheme."
(
(define-initial-key (list ctrl-x-char (integer->char 3)) procedure) ;;;C-X C-C
)
  (%save-buffer-changes (current-buffer))

  ;;; the following five lines fix an error with vector index out of range
  ;;; in edwin using C-X ! to split screen, then using C-X C-C to exit edwin
  ;;; reenter edwin and try C-X ! then error occurs
  (if *split-screen-mode?*                                  ;;; 2/14/86
      (begin
         (set! *split-screen-mode?* #!false)
         (move-editor-to-full)
         (move-pcs-to-full)))

  (set! edwin-editor #!unassigned)
  (edwin-exit))

(define-initial-command-key ("^R Redraw Screen" argument)
  "Redraw the screen."
(
(define-initial-key (integer->char 12) procedure)      ;;; C-L
)
 (window-redraw! (current-window))
 (reset-modeline-window))

(define (edwin-exit)
  (restore-console-contents)
  (make-pcs-status-visible)
  (reset-typein-window)
  (gc)
  ((fluid editor-continuation) *the-non-printing-object*))


;;;; Command Argument Reader

;;;; Commands

(define-initial-command-key ("^R Universal Argument" argument)
  "Increments the argument multiplier and enters Autoarg mode."
(
(define-initial-key  (integer->char 21) procedure)      ;;; C-U
)
  (command-argument-increment-multiplier-exponent!)
  (enter-autoargument-mode!)
  (update-argument-prompt!)
  (read-and-dispatch-on-char))

(define-initial-command-key ("^R Argument Digit" argument)
  "Sets the numeric argument for the next command."
(
 (set! ^r-argument-digit-command procedure)
)
  (command-argument-accumulate-digit! (char-base (current-command-char)))
  (update-argument-prompt!)
  (read-and-dispatch-on-char))

(define-initial-command-key ("^R Negative Argument" argument)
  "Negates the numeric argument for the next command."
(
(set! ^r-negative-argument-command procedure)
)
  (command-argument-negate!)
  (update-argument-prompt!)
  (read-and-dispatch-on-char))

(define-initial-command-key ("^R Autoargument Digit" argument)
  "In Autoargument mode, sets numeric argument to the next command."
(
(define-initial-key  #\0 procedure)
(define-initial-key  #\1 procedure)
(define-initial-key  #\2 procedure)
(define-initial-key  #\3 procedure)
(define-initial-key  #\4 procedure)
(define-initial-key  #\5 procedure)
(define-initial-key  #\6 procedure)
(define-initial-key  #\7 procedure)
(define-initial-key  #\8 procedure)
(define-initial-key  #\9 procedure)
)
  ((if (autoargument-mode?)
       ^r-argument-digit-command
       ^r-insert-self-command)
   argument))

(define-initial-command-key ("^R Auto Negative Argument" argument)
  "In Autoargument mode, sets numeric sign to the next command."
(
(define-initial-key  #\- procedure)
)
  ((if (and (autoargument-mode?) (command-argument-beginning?))
       ^r-negative-argument-command
       ^r-insert-self-command)
   argument))

;;;(define-initial-command-key ("^R Autoargument" argument)
;;;  "Used to start a command argument and enter Autoargument mode."
;;;(#!false
;;;)
;;;  (%edwin-autoargument argument))

;;;; File Commands

(define-initial-command-key ("^R Visit File" argument)
  "Visit new file in selected buffer."
(
(define-initial-key  (list ctrl-x-char (integer->char 22)) procedure)
)                                                        ;;; C-X C-V
  (let ((buffer (current-buffer)))
    (let ((pathname
           (prompt-for-pathname "Visit File :")))
      (save-buffer-changes buffer)
      (read-buffer buffer pathname)))
  (setup-current-buffer-read-only! argument))

(define-initial-command-key ("^R Save File" argument)
  "Save visited file on disk if modified."
(
(define-initial-key  (list ctrl-x-char (integer->char 19)) procedure)
)                                    ;;; C-X C-S
  (save-file (current-buffer)))

(define-initial-command-key ("Write File" argument)
  "Store buffer in specified file."
(
(define-initial-key  (list ctrl-x-char (integer->char 23)) procedure)
)                                     ;;; C-X C-W
  (let ((buffer (current-buffer)))
    (write-buffer
     buffer
     (prompt-for-pathname "Write buffer to file :"))))

(define-initial-command-key ("Insert File" argument)
  "Insert contents of file into existing text."
(
(define-initial-key (list ctrl-x-char (integer->char 9)) procedure)
)                                         ;;; C-X C-I
  (let ((pathname
         (prompt-for-pathname
          "Insert File :")))
    (set-current-region! (insert-file (current-point) pathname))))

(define-initial-command-key ("Write Region" argument)
 " Write Region to a file."
(
(define-initial-key (list ctrl-x-char (integer->char 16)) procedure)
)                                         ;;; C-X C-P

 (let ((pathname (prompt-for-pathname "Put region into file :")))
   (write-region (make-region (current-point) (current-mark)) pathname)))



(define-initial-command-key ("^R Newline" argument)
  "Insert newline, or move onto blank line."
(
(define-initial-key  #\Return procedure)
)
  (cond ((not argument)
         (if (line-end? (current-point))
             (let ((m1 (line-start (current-point) 1 #!false)))
               (if (and m1 (line-blank? m1)
                        (let ((m2 (line-start m1 1 #!false)))
                          (and m2 (line-blank? m2))))
                   (begin (set-current-point! m1)
                          (delete-horizontal-space))
                   (insert-newlines 1)))
             (insert-newlines 1)))
        (else
         (insert-newlines argument))))


;;;; Motion Commands

(define-initial-command-key ("^R Beginning of Line" (argument 1))
  "Move point to beginning of line."
(
(define-initial-key  (integer->char 1) procedure)         ;;; C-A
)
  (set-current-point! (line-start (current-point) (-1+ argument) 'LIMIT)))

(define-initial-command-key ("^R Backward Character" (argument 1))
  "Move back one character."
(
(define-initial-key  (integer->char 2) procedure)                ;;; C-B
(define-initial-key (list alt-char (integer->char 75)) procedure);;; <-
(set! ^r-backward-character-command procedure)
)
  (move-thing mark- argument))

(define-initial-command-key ("^R End of Line" (argument 1))
  "Move point to end of line."
(
(define-initial-key  (integer->char 5) procedure)    ;;; C-E
)
  (set-current-point! (line-end (current-point) (-1+ argument) 'LIMIT)))

(define-initial-command-key ("^R Forward Character" (argument 1))
  "Move forward one character."
(
(define-initial-key  (integer->char 6) procedure)                 ;;; C-F
(define-initial-key (list alt-char (integer->char 77)) procedure) ;;; ->
(set! ^r-forward-character-command procedure)
)
  (move-thing mark+ argument))

(define-initial-command-key ("^R Goto Beginning" argument)
  "Go to beginning of buffer (leaving mark behind)."
(
(define-initial-key  (list meta-char #\<) procedure)  ;;; M-<
)                                                     ;;; alt is blocked
  (cond ((not argument)
         (set-current-point! (buffer-start (current-buffer))))
        ((command-argument-multiplier-only?)
         (set-current-point! (buffer-end (current-buffer))))
        ((and (<= 0 argument) (<= argument 10))
         (set-current-point! (region-10ths (buffer-region (current-buffer))
                                           argument)))))

(define-initial-command-key ("^R Goto End" argument)
  "Go to end of buffer (leaving mark behind)."
(
(define-initial-key  (list meta-char #\>) procedure) ;;; M-> alt is blocked
)
  (cond ((not argument)
         (set-current-point! (buffer-end (current-buffer))))
        ((and (<= 0 argument) (<= argument 10)
         (set-current-point! (region-10ths (buffer-region (current-buffer))
                                           (- 10 argument)))))))

(define (region-10ths region n)
  (mark+ (region-start region)
         (quotient (* n (region-count-chars region)) 10)
         #!false))


(define goal-column #!FALSE)

(define temporary-goal-column-tag
  "Temporary Goal Column")

(define (current-goal-column)
  (or goal-column
      (command-message-receive temporary-goal-column-tag
        identity-procedure
        (lambda () (mark-column (current-point))))))

;;; this is temporary as we have not put the image stuff.
;;; this redefines mark-column and make-mark-from-column in struct

(define mark-column
  (lambda (mark)
    (char->x (line-string (mark-line mark)) (mark-position mark))))

(define make-mark-from-column
  (lambda (line column)
    (let ((mark (%make-mark line (x->char (line-string line) column) #!true))
          (group (line-group line)))
      (cond ((mark< mark (%group-start group)) (%group-start group))
            ((mark> mark (%group-end group)) (%group-end group))
            (else mark)))))

(define-initial-command-key ("^R Down Real Line" (argument 1))
  "Move down vertically to next real line."
(
(define-initial-key  (integer->char 14) procedure)    ;;; C-N
(define-initial-key (list alt-char (integer->char 80)) procedure)
)
  (let ((column (current-goal-column)))
    (line-offset (mark-line (current-point))
                 argument
                 (lambda (line)
                   (set-current-point! (make-mark-from-column line column)))
                 (lambda (line)
                   (let ((buffer (current-buffer)))
                     (region-insert-newline! (buffer-end buffer))
                     (set-current-point! (buffer-end buffer)))))
    (set-command-message! temporary-goal-column-tag column)))

(define-initial-command-key ("^R Up Real Line" (argument 1))
  "Move up vertically to next real line."
(
(define-initial-key  (integer->char 16) procedure)     ;;; C-P
(define-initial-key (list alt-char (integer->char 72)) procedure)
)
  (let ((column (current-goal-column)))
    (line-offset (mark-line (current-point))
                 (- argument)
                 (lambda (line)
                   (set-current-point! (make-mark-from-column line column)))
                 (lambda (line)
                   (set-current-point! (buffer-start (current-buffer)))))
    (set-command-message! temporary-goal-column-tag column)))


