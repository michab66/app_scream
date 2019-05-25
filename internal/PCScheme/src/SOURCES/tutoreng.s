;;; =============================================
;;; The Tutorial Engine
;;;
;;;	 Bob Beal
;;;
;;; Copyright 1986,1987 (c) Texas Instruments
;;; =============================================


;;; Auxiliary macros =========================

;; these might be useful anywhere

;; form: (push value var)
;; push "value" onto list stored at "var"
;; not a generalized-variable push
(macro push
  (lambda (e)
    (let ((value (cadr e))
	  (var (caddr e)))
      `(set! ,var (cons ,value ,var)))))

;; form: (in-bounds? low value high)
;; tests "low" <= "value" < "high"
(macro in-bounds?
  (lambda (e)
    (let ((lo (cadr e))
	  (x (caddr e))
	  (hi (cadddr e)))
      `(and (<=? ,lo ,x) (<? ,x ,hi)))))

;;; data structure definitions =========================

(define-structure tutorial
		  (name "")
		  (writeln-extensions do-nothing)
		  (frame-list nil)
		  (visited-list nil)
		  (frame-number nil)
		  (name-list nil)
		  (tc nil)
		  (index nil)
		  )

; arg is a "frame"
(macro frame-name	  (lambda (e) `(list-ref ,(cadr e) 1)))
(macro frame-lines-before (lambda (e) `(list-ref ,(cadr e) 2)))
(macro frame-item	  (lambda (e) `(list-ref ,(cadr e) 3)))
(macro frame-lines-after  (lambda (e) `(list-ref ,(cadr e) 4)))
(macro frame-dependencies (lambda (e) `(list-ref ,(cadr e) 5)))
(macro frame-tc-entry	  (lambda (e) `(list-ref ,(cadr e) 6)))
(macro frame-index-entry  (lambda (e) `(list-ref ,(cadr e) 7)))
(macro frame?		  (lambda (e) `(eq? (car ,(cadr e)) 'frame)))
; A data-driven SET! would be preferable to the following.
(macro set-frame-name!	  (lambda (e) `(set-car! (cdr ,(cadr e)) ,(caddr e))))


;;; Shorthand expressions for common idioms =========================

;; for arbitrary frames -------------------------

;; form: (nth-frame number)
(macro nth-frame
  (lambda (e)
    (let ((n (cadr e)))
      `(vector-ref (tutorial-frame-list *tutorial*) ,n))))

;; form: (frame-visited? frame)
(macro frame-visited?
  (lambda (e)
    (let ((e (cadr e)))
      `(vector-ref (tutorial-visited-list *tutorial*)
		   (frame->number ,e)))))

;; form: (set-frame-visited! frame true-or-false)
(macro set-frame-visited!
  (lambda (e)
    (let ((e (cadr e)) (value (caddr e)))
      `(vector-set! (tutorial-visited-list *tutorial*)
		    (frame->number ,e)
		    ,value))))

;; form: (frame->number frame)
;; given a frame, return its number
(macro frame->number
  (lambda (e)
    (let ((e (cadr e)))
       `(cdr (assq (frame-name ,e) (tutorial-name-list *tutorial*))))))

;; form: (name->frame name)
;; given a frame name, return its frame
(macro name->frame
  (lambda (e)
    (let ((name (cadr e)))
       `(nth-frame (cdr (assq ,name (tutorial-name-list *tutorial*)))))))

;; for the executing tutorial -------------------------

;; form: (unstarted-tutorial?)
;; has this tutorial been run since loading?
(macro unstarted-tutorial?
  (lambda (e)
    '(not (vector? (tutorial-frame-list *tutorial*)))))

;; form: (tutorial-length)
;; returns the number of frames in the tutorial
(macro tutorial-length
  (lambda (e)
    '(vector-length (tutorial-frame-list *tutorial*))))

;; form: (frame-list)
;; returns the tutorial's frame-list
(macro frame-list
  (lambda (e)
    '(tutorial-frame-list *tutorial*)))

;; form: (frame-number)
;; returns the frame-number of the current frame
(macro frame-number
  (lambda (e)
    '(tutorial-frame-number *tutorial*)))

;; form: (current-frame)
;; returns the current frame
(macro current-frame
  (lambda (e)
    '(vector-ref (tutorial-frame-list *tutorial*)
		 (tutorial-frame-number *tutorial*))))

;; form: (demo-writeln-extensions)
;; returns the function that handles text in a text zone
(macro demo-writeln-extensions
  (lambda (e)
    `(tutorial-writeln-extensions *tutorial*)))

;; this macro defines one "frame" -------------------------

(macro frame
  (lambda (e)
    `(push ',e (tutorial-frame-list *tutorial*))))

(macro frame-during-edit
  (lambda (e)
    `(set! *frame* ',e)))

;; for popup windows (menus, help screens) -------------------------

;; form: (with-popup-window dummy-window-var
;;	    TITLE string
;;	    TEXT-ATTRIBUTES attributes
;;	    BORDER-ATTRIBUTES attributes
;;	    POSITION (row . column)
;;	    SIZE (rows . columns)
;;	    &BODY &body)
;; The keywords aren't evaluated but the associated values are.
(macro with-popup-window
  (lambda (e)
    (let ((w (cadr e))
	  (title (cadr (memq 'title e)))
	  (text-attributes (cadr (memq 'text-attributes e)))
	  (border-attributes (cadr (memq 'border-attributes e)))
	  (position (cadr (memq 'position e)))
	  (size (cadr (memq 'size e)))
	  (body (cdr (memq '&body e))))
      `(let ((,w (make-window ,title #!true)))
	 ,(when text-attributes
		`(window-set-attribute! ,w 'text-attributes ,text-attributes))
	 ,(when border-attributes
		`(window-set-attribute! ,w 'border-attributes ,border-attributes))
	 ,(when position
		`(window-set-position! ,w (car ,position) (cdr ,position)))
	 ,(when size
		`(window-set-size! ,w (car ,size) (cdr ,size)))
	 (window-popup ,w)
	 (begin0
	   (begin ,@body)
	   (window-popup-delete ,w))))))

;; other -------------------------

;; form: (center-at msg)
;; returns the column at which cursor must be positioned to
;; center msg on console window
(macro center-at
  (lambda (e)
    (let ((msg (cadr e)))
      `(- 40 (floor (/ (string-length ,msg) 2))))))

;;; Auxiliary functions =========================

(define ATTR
  (let ((attrs-ibm '((blink . 128) (bkg-white . 112)
		     (bkg-brown . 96) (bkg-magenta . 80) (bkg-cyan . 48)
		     (bkg-red . 64) (bkg-green . 32) (bkg-blue . 16)
		     (light-white . 15)
		     (yellow . 14) (light-magenta . 13) (light-red . 12)
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
		      (LAMBDA (X)
			(AND (ASSOC X ATTRS)
			     (SET! ACC (+ ACC (CDR (ASSOC X ATTRS)))))) X)))
		(and (=? pcs-machine-type 1)   ;keep text enabled in TI mode
		     (=? acc prime-ti)
		     (set! acc default))
		acc)))
	(if (=? pcs-machine-type 1)
	    (work-fn attrs-ti  default-attrs-ti  prime-ti)
	    (work-fn attrs-ibm default-attrs-ibm prime-ibm))))))

(define demo-writeln
  (lambda (x w) 	     ;x=string of >=1 words, w=window
    (mapc (lambda (word)
	    (cond (((demo-writeln-extensions) word w))
		  (else (display word w))))
	  (let loop ((word-list nil) (s x))
	    (let ((n (substring-find-next-char-in-set s 0 (string-length s) " ")))
	      (cond (n (loop (cons (substring s 0 (1+ n)) word-list)
			     (substring s (1+ n) (string-length s))))
		    (else (reverse (cons (string-append s " ") word-list)))))))))

;; a "filler" function
(define (do-nothing . x) nil)

;(define visited
;  (lambda ()
;    (vector->list (tutorial-visited-list *tutorial*))))

;;; Advertised public interface =========================

;; Global variables -------------------------

(define *data-item*)
(define *evaled-data-item*)
(define *tutorial*)
(define *auto-tutorial?* nil)
(define *debug-tutorial* nil)	     ;not advertised
(define *frame* nil)		     ;	 "

;; Exported functions -------------------------

(define start-tutorial)
(define resume-tutorial)

;;; the tutorial "engine" =========================

(letrec
  ((alert
     (lambda (msg)
       (with-popup-window w
	 title ""
	 size `(1 . ,(string-length msg))
	 position `(5 . ,(center-at msg))
	 border-attributes (attr 'red)
	 text-attributes (if (=? pcs-machine-type 1)
			     (attr 'red 'reverse)
			     (attr 'black 'bkg-red))
	 &body
	 (beep)
	 (display msg w)
	 (read-char))))
   (banner
     (lambda ()
       (window-clear 'console)
       (with-popup-window w
	 title ""
	 size '(22 . 78)
	 position '(1 . 1)
	 &body
	 (let ((clear-msg "Press any key to continue.")
	       (banner
		 `("Texas Instruments"
			  "proudly presents:"
			  ""
			  "A PC Scheme Tutorial"
			  "on"
			  ,@(cond ((string? (tutorial-name *tutorial*))
				   (list (tutorial-name *tutorial*)))
				  ((pair?   (tutorial-name *tutorial*))
				   (tutorial-name *tutorial*))
				  (else
				    (list "The Reliance of Programming on Thaumaturgy"))))))
	   (window-set-cursor! w 3 1)
	   (for-each (lambda (s)
		       (window-set-cursor!
			 w
			 (car (window-get-cursor w))
			 (center-at s))
		       (print s w)
		       (newline w))
		     banner)
	   (window-set-cursor!
	     w
	     21
	     (center-at clear-msg))
	   (display clear-msg w)
	   (tutorial-read-char)))))
   (beep
     (lambda ()
       (display (integer->char 7))))
   (busy-window
     (let ((w (make-window nil nil)))
       (window-set-size! w 1 20)
       (window-set-attribute! w 'text-attributes (attr 'green 'blink))
       w))
   (calc-zone
     (lambda (e)
       (window-set-attribute! 'console 'text-attributes (attr 'green))
       (clear-rest-of-visited-list (frame->number e))  ;force reanalysis of environment
       (execute-frame-item e #!true eval?)
       (fresh-line)
       (newline)))
   (clear-rest-of-visited-list
     (lambda (n)
       (cond ((>=? n (tutorial-length)))
	      (else
	       (vector-set! (tutorial-visited-list *tutorial*) n #!false)
	       (clear-rest-of-visited-list (1+ n))))))
   (clear-visited-list
     (lambda ()
       (vector-fill! (tutorial-visited-list *tutorial*) nil)))
   (collect-index
     (lambda ()
       (set! (tutorial-index *tutorial*)
	     (sort!
	       (let loop ((n 0) (acc nil))
		 (cond ((>=? n (tutorial-length)) acc)
		       (else
			 (for-each (lambda (string)
				     (let ((index-item (assoc string acc)))
				       (cond (index-item
					      (append! index-item (list n)))
					     (else
					      (push (list string n) acc)))))
				   (frame-index-entry (nth-frame n)))
			 (loop (1+ n) acc))))
	       (lambda (x y)
		 (string-ci<? (car x) (car y)))))))
   (collect-names
     (lambda ()
       (let loop ((n 0) (acc nil))
	 (cond ((>=? n (tutorial-length))
		(set! (tutorial-name-list *tutorial*) acc))
	       ((frame-name (nth-frame n))
		(loop (1+ n) (cons (cons (frame-name (nth-frame n))
					 n)
				   acc)))
	       (else   ;give it a name and try again
		(set-frame-name! (nth-frame n) (gensym))
		(loop n acc))))))
   (collect-tc
     (lambda ()
       (set! (tutorial-tc *tutorial*)
	     (sort!
	       (let loop ((n 0) (acc nil))
		 (cond ((>=? n (tutorial-length))
			acc)
		       ((frame-tc-entry (nth-frame n))
			(loop (1+ n)
			      (cons (list n (frame-tc-entry (nth-frame n))) acc)))
		       (else
			 (loop (1+ n) acc))))))
       (when (>=? (length (tutorial-tc *tutorial*)) 21)
	     (error "Only 20 entries are allowed in the tutorial table of contents."))))
   (continue
     (lambda ()
       (let ((bad-key-msg "Invalid key pressed. \"?\" provides help."))
	 (fresh-line)
	 (display (integer->char 2))
	 (let again ((ch (tutorial-read-char)))
	   (case ch
	     (#\? (again (help)))
	     (#\backspace nil)
	     ((#\e #\E) (again (if *debug-tutorial*
				   (edit)
				   (alert bad-key-msg))))
	     ((#\i #\I) (index))
	     ((#\p #\P) (again (previous-frame)))
	     ((#\q #\Q) (quit))
	     ((#\return #\space #\n #\N) (again (next-frame)))
	     ((#\t #\T) (table-of-contents))
;	      (nil nil)     ;this doesn't work for some reason
	     (#!true nil)   ;so use this instead
	     (else (again (alert bad-key-msg))))))))
   (display-title-window
     (let ((blanks (make-string 15 #\space)))
       (lambda ()
	 (window-clear title-window)
	 (display blanks title-window)
	 (print (frame-number) title-window)
	 (print blanks title-window)
	 (when (frame-tc-entry (current-frame))
	       (demo-writeln (frame-tc-entry (current-frame)) title-window)
	       (fresh-line title-window)
	       (newline title-window)))))
   (do-tutorial
     (named-lambda (loop)
       (frame-1 (current-frame))
       (loop)))
   (edit
     (lambda ()
       (let ((prev-defn (getprop 'frame 'pcs*macro)))
	 (putprop 'frame (getprop 'frame-during-edit 'pcs*macro) 'pcs*macro)
	 (begin0
	   (with-popup-window
	     w
	     title "Edit menu"
	     size '(12 . 34)
	     position '(3 . 45)
	     &body
	     (print (assq (frame-name (current-frame)) (tutorial-name-list *tutorial*)) w)
	     (print (string-append "Frame evaluation is: " (if eval? "ON" "OFF")) w)
	     (print "" w)
	     (print "E - call Edwin" w)
	     (print "R - replace" w)
	     (print "T - new toplevel" w)
	     (print "V - toggle frame evaluation" w)
	     (print "and all standard keys" w)
	     (print "" w)
	     (let again ((ch (read-char)))
	       (case ch
		 ((#\e #\E)
		  (edwin)
		  (again (read-char)))
		 ((#\r #\R)
		  (cond ((frame? *frame*)
			 (set-frame-name! *frame* (frame-name (current-frame)))
			 (set! (current-frame) *frame*)
			 #!true)
			(else
			 (alert "Frame has bad format. Replace not done."))))
		 ((#\t #\T)  ;will this work?  YES!!
		  (beep)
		  (print "((fluid q)) quits new toplevel" w)
		  (let ((prev-history (getprop '%pcs-stl-history %pcs-stl-history)))
		    (call/cc
		      (lambda (k)
			(fluid-let ((scheme-top-level nil)
				    (q (lambda () (k 'end-top-level))))
;			   (set! pcs-gc-reset "((fluid q)) quits new toplevel")
			  (reset-scheme-top-level)
			  (reset))))
		    (set! pcs-gc-reset nil)
		    (putprop '%pcs-stl-history prev-history %pcs-stl-history)
		    #!true))
		 ((#\v #\V)
		  (set! eval? (not eval?))
		  #\E)	;force redisplay of edit menu
		 (else ch))))
	 (putprop 'frame prev-defn 'pcs*macro)))))
   (end-frame
     '(frame
	()
	("You have reached the end of the tutorial."
	 "Please press \"Q\" to exit.")))
   (eval? #!true)   ;var used in edit mode
   (execute-frame-item
     (lambda (e print? eval?)
       (cond ((eq? (frame-visited? e) #!true))
	     ((null? (frame-dependencies e))
	      (frame-item-parser (frame-item e) print? eval?)
	      (set-frame-visited! e #!true))
	     (else
	      (when print?
		    (window-set-position! busy-window
					  (car (window-get-cursor 'console))
					  0)
		    (window-popup busy-window)	;popdown when output occurs
		    (display "Evaluating..." busy-window))
	      (for-each (lambda (e)
			  (set! e (name->frame e))
			  (execute-frame-item e #!false eval?))
			(frame-dependencies e))
;	       (when print?
;		     (window-popup-delete busy-window))
	      (frame-item-parser (frame-item e) print? eval?)
	      (set-frame-visited! e #!true)))))
   (frame-1
     (lambda (e)
       (window-clear 'console)
       (display-title-window)
       (when (frame-lines-before e) (text-zone (frame-lines-before e)))
       (when (frame-item e)	    (calc-zone e))
       (when (frame-lines-after e)  (text-zone (frame-lines-after e)))
       (continue)))
   (frame-item-parser
     (lambda (cmds print? eval?)
       (let loop ((cmds cmds))
	 (cond ((null? cmds))
	       (else
		 (case (car cmds)
		   (:data (set! *data-item* (cadr cmds))
			  (set! cmds (cdr cmds)))
;		    (:read (set! *data-item* (read data-port)))
		   (:data-eval
		    (when eval? (set! *evaled-data-item* (eval *data-item*))))
		   (:eval
		    (when eval? (eval (cadr cmds)))
		    (set! cmds (cdr cmds)))
;		    (:skip (read data-port))
		   ((:pp-data :pp-evaled-data :yields :fresh-line :output)
		    (when print?
			  (window-popup-delete busy-window)  ;popdown busy msg
			  (case (car cmds)
			    (:output (when eval? (eval (cadr cmds)))
				     (set! cmds (cdr cmds)))
			    (:pp-data (pp *data-item*))
			    (:pp-evaled-data (pp *evaled-data-item*))
			    (:yields (display "  --->  "))
			    (:fresh-line (fresh-line)))))
		   (else nil))
		 (loop (cdr cmds)))))))
   (help
     (lambda ()
       (with-popup-window w
	 title "Help menu"
	 size '(12 . 34)
	 position '(3 . 45)
	 &body
	 (print "? - This menu" w)
	 (print "BACKSPACE - refresh screen" w)
	 (when *debug-tutorial*
	       (print "E - edit tutorial" w))
	 (print "I - index" w)
	 (print "N, RETURN, SPACE - next frame" w)
	 (print "P - previous frame" w)
	 (print "T - table of contents" w)
	 (print "Q - quit tutorial" w)
	 (read-char))))
   (index
     (lambda ()
       (let ((prompt-msg "Please type a frame number, nil, U, or D, then RETURN: "))
	 (with-popup-window
	   w
	   title "Index"
	   size '(22 . 78)
	   position '(1 . 1)
	   &body
	   (let show-one-page ((n 0))
	     (window-clear w)
	     (let vloop ((start (list-tail (tutorial-index *tutorial*) n))
			 (end (list-tail (tutorial-index *tutorial*) (+ n 20))))
	       (cond ((eq? start end))
		     (else
		      (display "     " w)
		      (display (caar start) w)
		      (let hloop ((tab-to 27)
				  (frame-no-list (cdar start)))
			(cond ((null? frame-no-list))
			      (else
			       (tab (current-column w) tab-to 4 w)
			       (display (car frame-no-list) w)
			       (display " " w)
			       (hloop (+ tab-to 4) (cdr frame-no-list)))))
		      (newline w)
		      (vloop (cdr start) end))))
	     (window-set-cursor! 'console 22 (center-at prompt-msg))
	     (display prompt-msg)
	     (let ((frame-no (read)))
	       (flush-input)
	       (cond ((and (number? frame-no)
			   (in-bounds? 0 frame-no (tutorial-length)))
		      (clear-visited-list)
		      (set! (frame-number) frame-no))
		     ((eq? frame-no 'U)
		      (show-one-page (if (<? (- n 20) 0) 0 (- n 20))))
		     ((eq? frame-no 'D)
		      (show-one-page (if (>=? (+ n 20) (length (tutorial-index *tutorial*)))
					 n
					 (+ n 20))))
		     ((and *debug-tutorial*
			   (assq frame-no (tutorial-name-list *tutorial*)))
		      (clear-visited-list)
		      (set! (frame-number) (cdr (assq frame-no (tutorial-name-list *tutorial*))))))
	       #!true))))))
   (init-tutorial
     (lambda (tutorial resume)
       (when (not (equal? *debug-tutorial* '(#\?)))  ;make it harder to enter debug mode
	     (set! *debug-tutorial* nil))
       (when tutorial
	     (set! *tutorial* tutorial))
       (when (not (tutorial? *tutorial*))
	     (alert "There is no tutorial available.")
	     (quit))
       (when (and (unstarted-tutorial?)
		  resume)
	     (alert "You cannot resume an unstarted tutorial. Use (START-TUTORIAL).")
	     (quit))
       (when (unstarted-tutorial?)
	     (set! (frame-list)
		   (list->vector (cons start-frame
				       (reverse! (cons end-frame
						       (frame-list))))))
	     (set! (tutorial-visited-list *tutorial*)
		   (make-vector (vector-length (frame-list))))
	     (set! (frame-number) 0)
	     (set! eval? #!true)
	     (collect-names)
	     (collect-tc)
	     (collect-index))
       (begin			   ;make sure entire screen gets erased
	 (window-set-position! 'console 0 0)
	 (window-set-size! 'console 24 80)  ;leave status line
	 (window-set-attribute! 'console 'text-attributes (attr))
	 (window-clear 'console))
       (when (not resume)
	     (banner)
	     (set! (frame-number) 0)
	     (clear-visited-list))
       (call/cc
	 (lambda (k)
	   (set! quit-k (lambda ()
			  (k nil)))
	   (call/cc (lambda (k)
		      (set! *user-error-handler*
			    (lambda x (user-error-handler k)))))
	   (do-tutorial)))))
   (next-frame
     (lambda ()
       (if (=? (frame-number)
	       (-1+ (tutorial-length)))
	   (if *auto-tutorial?*
	       #\q
	       (alert "You are on the last frame of the tutorial."))
	   (begin (set! (frame-number) (1+ (frame-number)))
		  #!true))))
   (previous-frame
     (lambda ()
       (if (zero? (frame-number))
	   (alert "You are on the first frame of the tutorial.")
	   (begin (set! (frame-number) (-1+ (frame-number)))
		  #!true))))
   (print
     (lambda (x w)
       (display x w)
       (newline w)))
   (quit
     (lambda ()
       (window-clear 'console)
       (set! *user-error-handler* nil)
       (quit-k)))
   (quit-k reset)	     ;the quit continuation
			     ;reassigned by init-tutorial
   (start-frame
     '(frame
	()
	()
	(:data "A PC Scheme Tutorial" :pp-data)
	("The \"?\" is the help key."
	      "It displays a menu which tells you"
	      "about other important keys which enable you"
	      "to move around in the tutorial or to leave it."
	      "\"?\" or other single-keystroke keys are available"
	      "anytime you see the \"happy-face\" character towards"
	      "the bottom of the screen."
	      "Occasionally, typed input is requested."
	      "Typed input is"
	      "usually a number, or the atom NIL, followed by"
	      "the RETURN key."
	      "If you exit the tutorial in the middle, you can"
	      "continue from where you left off"
	      "(in the same session)"
	      "by typing (RESUME-TUTORIAL)."
	      "An \"Evaluating...\" message may appear while the"
	      "tutorial establishes"
	      "the proper execution environment for the examples in that"
	      "frame.")
	()
	"Directions for running the tutorial"
	("directions for running tutorial")))
   (tab
     (lambda (cur goal multiple w)
       (cond ((<? cur goal)
	      (display " " w)
	      (tab (+ cur 1) goal multiple w))
	     ((=? cur goal)
	      cur)
	     (else
	      (tab cur (+ goal multiple) multiple w)))))
   (table-of-contents
     (lambda ()
       (let ((prompt-msg "Please type a frame number or nil then RETURN: "))
	 (with-popup-window
	   w
	   title "Table of Contents"
	   size '(22 . 78)
	   position '(1 . 1)
	   &body
	   (print "     Frame#          Subject" w)
	   (for-each (lambda (chapter-title)
		       (let ((n (car chapter-title))
			     (title (cadr chapter-title)))
			 (display "        " w)
			 (display n w)
			 (display "        " w)
			 (display title w)
			 (newline w)))
		     (tutorial-tc *tutorial*))
	   (window-set-cursor! 'console 22 (center-at prompt-msg))
	   (display prompt-msg)
	   (let ((frame-no (read)))
	     (flush-input)
	     (cond ((and (number? frame-no)
			 (in-bounds? 0 frame-no (tutorial-length)))
		    (clear-visited-list)
		    (set! (frame-number) frame-no))
		   ((and *debug-tutorial*
			 (assq frame-no (tutorial-name-list *tutorial*)))
		    (clear-visited-list)
		    (set! (frame-number) (cdr (assq frame-no (tutorial-name-list *tutorial*))))))
	     #\backspace)))))
   (text-zone
     (lambda (lines)
       (window-set-attribute! 'console 'text-attributes (attr))
       (set-line-length! 55 'console)
       (for-each (lambda (line) (demo-writeln line 'console)) lines)
       (set-line-length! 80 'console)
       (fresh-line)
       (newline)))
   (title-window
     (let ((w (make-window nil nil)))
       (window-set-position! w 0 60)
       (window-set-size! w 10 20)
       (window-set-attribute! w 'text-attributes (attr 'cyan))
       w))
   (tutorial-read-char
     (lambda ()
       (if *auto-tutorial?* #\space (read-char))))
   (user-error-handler
     (lambda (k)
       (alert "System error in this frame.")
       (if *debug-tutorial*
	   (set! eval? #!false)      ;debugging, stay on current frame
	   (next-frame))	     ;else go on to next frame
       (k nil)))
   )
  (set! (access frame-1 user-initial-environment) frame-1)
  (set! start-tutorial
	(lambda which
	  (init-tutorial (car which) nil)))
  (set! resume-tutorial
	(lambda which
	  (init-tutorial (car which) 'resume))))

