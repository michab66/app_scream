;;; Tutorial Engine tutorial
;;; Copyright 1987 (c) Texas Instruments


;;; This is the tutorial text to the Tutorial Engine tutorial.


; To run this tutorial, first compile and fasl the file "tutoreng.s".
; Then do the following:
;
;   (load "tutoreng.fsl")       ;load the Tutorial Engine program
;   (load "tutframe.s")         ;load this tutorial
;   (start-tutorial)		;start it

; If you prefer a paper tutorial to an online one, you can do the following:
;   (set! *auto-tutorial?* #t)
;   (transcript-on "<some file here>")
;   (start-tutorial)
;   ; the tutorial runs by itself; when it finishes:
;   (transcript-off)
; The tutorial has been captured on the transcript file.
; *Auto-tutorial?* does not create output suitable for a book, but
; the results are readable.

; If you're wondering why the SCOOPS tutorial isn't run the
; same way, it could have been, but it is packaged differently.
; The Tutorial Engine program, tutorial text, and graphics demo
; were combined together in one fasl file.  Contrary to a statement
; in one of the manuals, this is easy to do; just use DOS COPY to
; concatenate the fasl files together, but be sure to specify
; the /b (binary) option to avoid early truncation.


(define extensions
  (lambda (word window)
    (let ((c (string-ref word 0)))
      (case c
	(#\/ (window-set-attribute! window 'text-attributes (attr 'yellow))
	     (display (substring word 1 (string-length word))))
	(#\\ (window-set-attribute! window 'text-attributes (attr))
	     (display (substring word 1 (string-length word))))
	(else (display word window))))))



;;; the tutorial's frames ----------------------------------------

(set! *tutorial*
  (make-tutorial
    'name "The Tutorial Engine"
    'writeln-extensions extensions))

(frame
  ()
  ("The /Tutorial Engine \\is a program that"
   "implements a simple model of tutorial interaction."
   "This permits the interaction to be embodied in the program itself,"
   "but the tutorial text is separate from the program, and many different"
   "tutorial texts can be used with the program.")
  ()
  ("There are added advantages to the tutorial writer."
   "Text is automatically formatted so you don't have to,"
   "and examples are executed directly so you don't have to"
   "capture input and output values and format them yourself."
   "The current presentation format is admittedly biased towards"
   "displaying Scheme code.")
  ()
  "Introduction")

(frame
  ()
  ("The model is one familiar to most people: the slide show."
   "A /tutorial \\(slide show) consists of a series of /frames \\(slides)."
   "Normally, you progress through the frames in a forward direction,"
   "but you can skip around."
   "A frame concentrates on one topic, or /example, \\with"
   "explanatory text surrounding the example.")
  ()
  ("Unlike a slide show, you interact with the tutorial."
   "Therefore, various kinds of assistance"
   "are available. A /help \\window lists the single-keystroke commands and"
   "what they do. The /table of contents \\displays the topics covered by"
   "the tutorial, gives the frame number at which they start,"
   "and permits you to move around randomly in the tutorial."
   "The /index \\displays terms and phrases in alphabetical order,"
   "lists their frame numbers,"
   "and also allows you to skip around in the tutorial.")
  ()
  ()
  ("tutorial" "example" "frame" "help" "table of contents" "index"))

(frame
  initial
  ("Your view of a frame, as a user, is one screen containing"
   "text introducing the topic or example of the frame,")
  (:eval (display "the topic itself, set off from the surrounding text")
   :fresh-line
   :eval (display "and highlighted in green,"))
  ("and text afterwards explaining the example.")
  ()
  "Frames")

(frame
  ()
  ("From the Tutorial Engine's point of view,"
   "a frame is conceptually a Scheme structure but is implemented as a list."
   "Macros are used to hide this implementation from the rest of the program."
   "The frame format looks like this:")
  (:eval (display '(frame name before-text example after-text
	  dependencies tc-entry index-entries)))
  ("The FRAME keyword starts each frame. 'name' is an optional symbol"
   "that can be referenced by the dependency lists of other frames."
   "'before-text' and 'after-text' are lists of strings of text."
   "'tc-entry' consists of a string of text to be placed in the"
   "tutorial's table of contents."
   "'index-entries' is a list of strings; each string should be a word"
   "or short phrase that would be appropriate to put into an index."
   "Subsequent frames discuss the 'example' and 'dependencies' entries.")
  ()
  ()
  ("frame"))

(frame
  ()
  ("The 'example' field is a list"
   "of /keyword \\or /keyword/value pairs \\representing"
   "Scheme expressions to be evaluated and displayed."
   "A keyword begins with a colon."
   "For example, the"
   "following description in the first line below"
   "generates the output in the second line:")
  (:eval (display '(:data (+ 3 5) :data-eval :pp-data :yields :pp-evaled-data))
   :eval (begin (fresh-line) (newline))
   :data (+ 3 5) :data-eval :pp-data :yields :pp-evaled-data)
  ("/:DATA \\records the text of the data. /:DATA-EVAL \\evaluates the data."
   "/:PP-DATA \\pretty-prints the data itself while //:PP-EVALED-DATA"
   "\\pretty-prints its result. /:YIELDS \\prints an arrow."
   "/:EVAL \\(not shown above) evaluates an arbitrary Scheme expression,"
   "and there are other keywords too."
   "Note that with this feature, examples are active items and not"
   "just passive pieces of text--the examples are actually executed"
   "during the running of a tutorial.")
  ()
  ()
  ("example"))

(frame
  ()
  ("The last field of a frame to be discussed are the 'dependencies.'"
   "This is a list of frame names on which this frame depends."
   "Since the examples are actually executed, and since the user"
   "can go to any frame at will, any set-up for the examples"
   "in that frame would likely be bypassed without this feature.")
  ()
  ("This approach, while flexible, has its limitations."
   "The primary one is speed. Straight text examples take more work"
   "to generate, but text displays are fast. Because dependencies"
   "have to be evaluated, if there are many of them, or if they involve"
   "time-consuming computations, it may take awhile to display the result."
   "Also, it is tricky getting their ordering correct.")
  ()
  ()
  ("dependencies"))

(frame
  ()
  ("A tutorial is not complete without two more things."
   "The first is to define a /print function \\that prints individual words,"
   "possibly changing screen attributes (color, reverse video, etc.)"
   "along the way.  The function takes 2 arguments: a word, which is"
   "a string, and a window in which to print the string."
   "Examining the source of this tutorial text should make its"
   "structure clear.")
  ()
  ("The important thing to note is that this function is /not \\part"
   "of the Tutorial Engine but belongs to the tutorial itself."
   "Different tutorials can use different printing functions,"
   "giving some variety in how frames are displayed,"
   "while still working within the model used by the Tutorial Engine.")
  ()
  "Tutorial Structure"
  ("print function"))

(frame
  ()
  ("The second is to create a /tutorial structure \\and assign"
   "it to *TUTORIAL*."
   "Unlike a frame, this is a true Scheme structure, and it has these fields:")
  (:eval (display '(name write-extensions frame-list visited-list
	  frame-number name-list tc index)))
  ("You should initialize 2 fields: 'name', to a string with the name"
   "of the tutorial, and 'write-extensions', to the print function"
   "discussed in the previous frame.")
  ()
  ()
  ("tutorial structure"))

(frame
  ()
  ("The other fields are used during the running of a tutorial."
   "When a tutorial is read from disk, the frames are consed into a list."
   "Then the list is converted to an array and stored in 'frame-list'."
   "The 'frame-number' is the number of the frame currently visible."
   "When a frame is displayed, its position in 'visited-list'"
   "(really an array again) is marked true."
   "When you skip around in a tutorial, the visited list is used"
   "to determine if the frames on which this one depends"
   "have all been executed.")
  ()
  ("The 'name-list' is a list of pairs of individual frame names and"
   "corresponding frame numbers and is for debug purposes."
   "The 'tc' and 'index' are the values used in the table of contents"
   "and index, respectively. The former has the format:"
   "((frame# tc-entry) ...) arranged by increasing frame number,"
   "and the latter has a format:"
   "((index-entry frame# frame# ...)) sorted in alphabetical order."
   "These are determined once, when a tutorial is started."))

(frame
  ()
  ("The Tutorial Engine has two exported functions, /START-TUTORIAL"
   "\\and /RESUME-TUTORIAL. \\A LETREC encloses the Tutorial Engine's"
   "local functions. A brief summary of the local functions follows.")
  ()
  ()
  ()
  "Description of the Tutorial Engine Program"
  ("exported functions"))

(frame
  ()
  ("/START-TUTORIAL \\and /RESUME-TUTORIAL \\call /INIT-TUTORIAL."
   "\\The banner screen is displayed by /BANNER \\if the tutorial hasn't"
   "been run before in the current session."
   "The routine /COLLECT-TC \\organizes the table of contents using"
   "the TC fields of each frame,"
   "/COLLECT-INDEX \\works similarly using each frame's INDEX field,"
   "and /COLLECT-NAMES \\looks at each frame's NAME field."
   "This last is for debugging and editing purposes."
   "Part of the initialization includes saving two continuations:"
   "/QUIT-K \\to exit the tutorial, and /USER-ERROR-HANDLER, \\which"
   "gets assigned to the system hook *USER-ERROR-HANDLER*,"
   "to recover from errors.")
  ()
  ()
  ()
  ()
  ("initialization"))

(frame
  ()
  ("/DO-TUTORIAL \\implements looping over each tutorial frame."
   "/FRAME-1 \\executes one frame of the tutorial."
   "/DISPLAY-TITLE-WINDOW \\displays the frame number and any"
   "table-of-contents title."
   "Displaying the 3 zones of before-text, example, and after-text"
   "is the job of the routines /TEXT-ZONE \\and /CALC-ZONE. \\")
  ()
  ("/CONTINUE \\handles all single-key input. It calls"
   "/NEXT-FRAME \\and /PREVIOUS-FRAME \\to move between frames,"
   "/HELP \\to display help information about single-key inputs,"
   "/TABLE-OF-CONTENTS \\to handle table-of-contents processing,"
   "ditto /INDEX \\for index processing, /QUIT \\to exit the tutorial"
   "by invoking the QUIT-K continuation, and /ALERT \\to display an"
   "error message in a pop-up window.")
  ()
  ()
  ("main loop" "keystroke handling"))

(frame
  ()
  ("/TEXT-ZONE \\is passed the list of strings to print."
   "/DEMO-WRITELN \\is called in turn with each string."
   "It breaks the string into individual words and calls"
   "the printing hook function of *TUTORIAL* to print each"
   "word as it sees fit. Filling the line is done automatically by Scheme."
   "The text zone widths are shrunk somewhat for esthetic reasons,"
   "and also the somewhat limited space forces the tutorial writer"
   "to be concise.")
  ()
  ()
  ()
  ()
  ("zone handling"))

(frame
  ()
  ("/EXECUTE-FRAME-ITEM \\parses and executes the example expressions"
   "in a frame. If the expressions depend on other expressions being"
   "executed first, it recursively calls itself to handle those frames first"
   "and puts up a /BUSY-WINDOW \\meanwhile. /FRAME-ITEM-PARSER \\is the"
   "workhorse function.")
  ()
  ()
  ()
  ()
  ("zone handling" "parsing"))

(frame
  ()
  ("/EDIT \\permits limited editing of a frame while a tutorial is running,"
   "assuming the global variable /*DEBUG-TUTORIAL* \\has been properly"
   "activated. The edit mode permits using Edwin to edit a frame and"
   "then replacing the current frame with the edited one in order to"
   "check on the appearance of the edited frame; this avoids having to"
   "recompile the entire Edwin buffer just to test a new frame."
   "Inserting or deleting frames is not implemented.")
  ()
  ("Evaluating a frame's example can be turned on and off from the edit"
   "mode. Evaluation errors automatically turn off frame evaluation so"
   "that the frame can be examined and edited. You can also go into"
   "a new system toplevel temporarily to test-evaluate examples.")
  ()
  ()
  ("edit mode"))

(frame
  ()
  ("Some of the LETREC variables are used for data. The tutorial's"
   "/START-FRAME \\and /END-FRAME \\are part of the Tutorial Engine itself"
   "and not in the tutorial text. /EVAL? \\controls executing a frame's"
   "example and is used in edit mode.")
  ()
  ()
  ()
  ()
  ("data values"))

(frame
  ()
  ("The Tutorial Engine is a complete Scheme program which demonstrates"
   "several useful Scheme programming techniques. Among these are using"
   "LETREC to /define local variables and functions \\which are hidden from"
   "the outside unless they are explicitly exported, like START-TUTORIAL,"
   "RESUME-TUTORIAL, and the rebinding of *USER-ERROR-HANDLER*."
   "A Scheme /structure \\is used to represent the tutorial"
   "and /macros \\hide the representation of a frame."
   "Macros are also used to extend the Scheme language, such as in"
   "WITH-POPUP-WINDOW, which defines a Common-Lisp-like form that"
   "uses keywords as part of its syntax.")
  ()
  ()
  ()
  "Scheme Techniques")

(frame
  ()
  ("/Continuations \\are used to implement exit and recovery points."
   "A named LET implements /looping \\in the local function CONTINUE."
   "/Window manipulations \\are demonstrated in many different places."
   "For example, ALERT pops up a small error message window, the BUSY-WINDOW"
   "is borderless, and TABLE-OF-CONTENTS and INDEX popup windows"
   "take over the entire screen."
   "FRAME-ITEM-PARSER shows how an /interpreter for a new language \\is"
   "build on top of Scheme through the use of EVAL."
   "Finally, with /lexical scoping \\the PRINT routine"
   "is redefined without affecting the system's PRINT routine."))

(frame
  ()
  ("A couple of tricks specific to PC Scheme are also demonstrated."
   "One is the creation of a /new toplevel."
   "\\The other is temporarily /redefining a frame's PCS*MACRO property \\so"
   "that a frame recompiled from Edwin can be redisplayed by the"
   "Tutorial Engine without requiring the recompilation of the entire"
   "tutorial text, which takes considerably longer."
   "Both of these occur inside EDIT."))

(frame
  ()
  ("This concludes our discussion of the Tutorial Engine."
   "The conceptual model that it implements of tutorial interaction is simple"
   "and can no doubt be expanded in many ways;"
   "maybe you will do so. At the least, you should find this complete example"
   "helpful in organizing your own Scheme programming.")
  (:data "Happy Scheming!!" :pp-data)
  ()
  ()
  "Conclusion")

