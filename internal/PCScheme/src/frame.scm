
(load "scoops.fsl")

(define extensions
  (let ((blanks (make-string 4 #\space)))
    (lambda (word w)	  ;word=string of 1 word followed by 1 blank
			  ;w=window
      (let ((c (string-ref word 0))
	    (word (substring word 1 (-1+ (string-length word)))))
	(case c
	  (#\/		;new term
	    (window-set-attribute! w 'text-attributes (attr 'yellow))
	    (display word w)
	    (window-set-attribute! w 'text-attributes (attr))
	    (display #\space w)
	    #!true)
	  (#\@		;emphasis
	    (window-set-attribute! w 'text-attributes (attr 'red))
	    (display word w)
	    (window-set-attribute! w 'text-attributes (attr))
	    (display #\space w)
	    #!true)
	  (#\!		;break
	    (fresh-line )
	    (display word w)
	    (display #\space w)
	    #!true)
	  (#\]		;break and tab
	    (fresh-line w)
	    (display blanks w)
	    (display word w)
	    (display #\space w)
	    #!true)
	  (else #!false))))))


;;; the tutorial's frames ----------------------------------------

(set! *tutorial*
  (make-tutorial
    'name "SCOOPS"
    'writeln-extensions extensions))


(frame
  initial
  ("This tutorial will take you through defining your own instances"
   "of SCOOPS classes and manipulating the instances. When the"
   "tutorial is finished you will have an opportunity to try your"
   "own hand at creating and manipulating SCOOPS classes. The"
   "classes for this tutorial are POINT, LINE and RECTANGLE."
   "Refer to chapter 5 in the Language Reference Manual for"
   "additional information on SCOOPS."))

(frame
  SCOOPS
  ("/SCOOPS is the /SCheme /Object /Oriented /Programming /System for PC Scheme,"
   "similar to the LOOPS and FLAVORS systems available on various"
   "makes of Lisp machines."
   "Object oriented programming"
   "involves the use of /objects as abstract data types. An object"
   "is comprised of /variables, which determine the local state of"
   "the object, and /methods which define the object's behavior.")
  ()
  ()
  ()
  "Introduction to SCOOPS"
  ("SCOOPS" "object-oriented programming"
   "object" "method"))

(frame
  ()
  ("In object oriented programming, all communication with an object"
   "is through /messages. Objects use their own"
   "procedures, called methods, to respond to the message and perform"
   "some operation. A key to object oriented programming is that the"
   "system performs many tasks that the programmer has to specify in"
   "other types of programming styles.")
  ()
  ()
  ()
  ()
  ("message" "method"))

(frame
  CLASS
  ("In our example the first thing that needs to be done with"
   "SCOOPS is to define a /class. A class contains the description"
   "of one or more similar objects. An object is an /instance of a class"
   "with the same form as the class from which it was made, a copy. Scheme"
   "uses the special form DEFINE-CLASS to create a class. For example:")
  (:data (define-class point (instvars (x 0) (y 0))) :data-eval :pp-data)

  ("This defines a class named POINT. Each instance of the class"
   "will contain two /instance /variables called X and Y and each is"
   "initialized to zero.")
  ()
  "Defining a Class"
  ("class" "DEFINE-CLASS" "instance variable" "instance"))

(frame
  DEFINE-POINT-CLASS
  ("This is a simple definition and has the disadvantage that"
   "when an instance is created it cannot be manipulated. No methods"
   "have been included to interact with the class. A small"
   "change to the definition is necessary to allow the variables"
   "to be changed.")
  (:data (define-class point (instvars (x 0) (y 0))
     (options  settable-variables)) :data-eval :pp-data)

  ("What this has done is to automatically define two methods for us,"
   "SET-X and SET-Y. A /method is a type of function or procedure that"
   "determines the behavior of a class. We will cover"
   "methods a little later.")
  ()
  ()
  ("method" "options"))

(frame
  DESCRIBE
  ("Now we can use the /DESCRIBE procedure. We can see that two"
   "methods have already been defined, SET-X and SET-Y. The"
   "DESCRIBE procedure can be used to describe either a class"
   "or an instance. For example if we describe the class \"point\""
   "with the command: (DESCRIBE POINT) the output will look like:")
  (:output (DESCRIBE POINT))
  ()
  ()
  "The DESCRIBE procedure"
  ("DESCRIBE"))

(frame
  ()
  ("This tells us several things:"
   "]- we're describing a class"
   "]- the class has no class variables"
   "(this tutorial won't be discussing them)"
   "]- there are two instance variables, X and Y"
   "]- two methods have been defined, SET-X and SET-Y"
   "]- there are no mixins"
   "]- the class is not compiled"
   "]- the class is not inherited"
   "]We haven't yet discussed mixins or inheritance. We will discuss those"
   "later. Compiling is the next topic."))

(frame
  COMPILE-CLASS
  ("Now that you have defined a class you should /compile it."
   "We're not actually generating code here but rather setting up"
   "the actual inheritance structure for a class; we'll discuss"
   "inheritance more later."
   "If you don't use COMPILE-CLASS, it will be compiled"
   "the first time you use the"
   "special form MAKE-INSTANCE. Continuing with our example:")
  (:data (COMPILE-CLASS POINT) :data-eval :pp-data)
  ()
  ()
  "Compiling a Class"
  ("compile" "COMPILE-CLASS" "inheritance"))

(frame
  MAKE-INSTANCE
  ("To create an instance of a class you would use the special form"
   "/MAKE-INSTANCE. A simple instance creation would be:")
  (:data (DEFINE P1 (MAKE-INSTANCE POINT)) :data-eval :pp-data)
  ("What this has done is to set up the data structure in memory"
   "for the instance using all defaults.")
  (define-point-class)
  "Creating an Instance of a Class"
  ("MAKE-INSTANCE" ))

(frame
  SEND
  ("In order to change the values of X and Y we would send a message to P1"
   "specifying the method we want to use to manipulate the data. For example,"
   "the command:")
  (:data (SEND P1 SET-X 50) :data-eval :pp-data)
  ("would change the value of X from 0, the initial value, to"
   "50.")
  (make-instance)
  "Sending Messages"
  ("SEND"))

(frame
  ()
  ("We can use the DESCRIBE procedure to describe P1 and examine the values"
   "of X and Y. This command would be: (DESCRIBE P1)")
  (:output (DESCRIBE P1))
  ("As you can see we are told we are describing an instance. The instance"
   "is of class POINT. There are no class variables."
   "The instance variables are X with a value of 50"
   "and Y with a value of 0. Which is what we would expect.")
  ()
  ()
  ("DESCRIBE"))

(frame
  DEFINE-METHOD
  ("To define a method for a class you use the special form"
   "/DEFINE-METHOD. Let's define a method to display the instances of"
   "the point class we've created. For example:")
  (:data (DEFINE-METHOD (POINT DRAW) () (DRAW-POINT X Y)) :data-eval :pp-data)
  ("What we would have to do now is to send two messages, one"
   "to change the value of X or Y and another to draw the point."
   "This would be fine if we only wanted to put points on the"
   "screen that were the same color and didn't mind old occurrences"
   "hanging around.")
  ()
  "Defining Methods"
  ("DEFINE-METHOD"))

(frame
  ()
  ("First we can modify the class definition to include color. This is"
   "simply adding another instance variable to be used to define the"
   "color. Our class POINT could now be defined as:")
  (:data (define-class point
		      (instvars (x 0)
				(y 0)
				(color 7))
		      (options	settable-variables))
      :data-eval :pp-data)
  ("Now we have another method defined for us, SET-COLOR. And we can"
   "manipulate the COLOR variable as we have manipulated the X variable."
   "The problem of having to send two messages, one to set the value and"
   "the other to draw the point still exists, however."))

(frame
  ACTIVE-VALUES
  ("We can modify the class definition to include /ACTIVE /VALUES."
   "Active values are used to trigger procedure invocations whenever"
   "the value of the variable is accessed or updated. The special form"
   "]\"(ACTIVE <initial-value> <get-fn> <set-fn>)\" !is used. Now when"
   "we use SET-X, SET-X will call the \"set-fn\" and perform whatever action"
   "that method indicates and will set the X to whatever value the"
   "\"set-fn\" returns. Our class definition is now:")
  (:data (define-class point
		      (instvars (x (active 0 () move-x))
				(y (active 0 () move-y))
				(color (active 7 () change-color))))
      :data-eval :pp-data)
  ("Active values are automatically gettable and settable so we don't need to"
   "specify those options.")
  ()
  "Active Values"
  ("active value"))

(frame
  ()
  ("Now when we send a message to P1 to set X to some"
   "value, the procedure MOVE-X is called automatically."
   "Of course we still need to"
   "write the procedures MOVE-X, MOVE-Y and CHANGE-COLOR.")
  (:data (compile-class point) :data-eval))


(frame
  MOVE-Y
  ("For example we will define the MOVE-Y method. First we will define"
   "an ERASE method to erase the previous position of the point and then"
   "we will define a REDRAW method to redraw the point in its new location.")
  (:data (define-method (point erase) () (set-pen-color! 'black)
      (draw-point x y)) :data-eval :pp-data :fresh-line
   :data (define-method (point redraw) () (set-pen-color! color)
      (draw-point x y)) :data-eval :pp-data :fresh-line
   :data (define-method (point move-y) (new-y) (erase) (set! y new-y)
      (redraw) new-y) :data-eval :pp-data)
  ())

(frame
  ()
  ("The methods for MOVE-X and CHANGE-COLOR would be very similar to MOVE-Y"
   "now that we have the ERASE and REDRAW methods."
   "We could, if we wanted, send a message to P1 and have the"
   "X value changed two ways. Either you can send a message to the"
   "MOVE-X method with a new value to which to set the variable or you"
   "can send a message to the SET-X method with a value and let Scheme"
   "call the MOVE-X method automatically.")
  (:data (define p1 (make-instance point)) :data-eval
   :data (send p1 move-y -50) :data-eval :pp-data :fresh-line
   :data (send p1 set-y -50) :data-eval :pp-data
   :data (send p1 erase) :data-eval)
  ("These two calls are equivalent since SET-Y will automatically call"
   "MOVE-Y.")
  (ACTIVE-VALUES MOVE-Y))

(frame
  INHERITANCE
  ("Another powerful feature of object oriented programming is"
   "/inheritance. Classes can inherit variables from previously"
   "defined classes. For example the class \"LINE\" can inherit the"
   "variables X, Y and COLOR from \"POINT\", and only need to define"
   "length and direction. For example:")
  (:data (define-class line
		       (instvars (len (active 50 () change-length))
				 (dir (active 0  () change-direction)))
		       (mixins point))
      :data-eval :pp-data)
  ("Remember that for active values there is no need to specify options."
   "The set and get methods are automatically generated. If we had some"
   "procedure to be performed by the get-function, besides returning the"
   "current value, then we could"
   "specify a method to be executed automatically by substituting the"
   "name where the \"()\" is before the set-function name.")
  ()
  "Inheritance"
  ("inheritance"))

(frame
  ()
  ("In addition to inheriting variables from other classes, methods"
   "are also inherited. This means that we do not have to define an"
   "erase method, we inherited it from \"POINT\". In fact the only methods"
   "we have to define are CHANGE-LENGTH, CHANGE-HEIGHT and DRAW."
   "We need our own draw method to draw a line instead of a point."
   "The practice of writing your methods to be as general as"
   "possible facilitates the inheritance feature.")
  ()
  ()
  ()
  ()
  ("inheritance"))

(frame
  ()
  ("Having defined the CHANGE-LENGTH and CHANGE-DIRECTION methods,"
   "we could modify the LINE by sending messages to the SET-LEN"
   "and SET-DIR methods. If we then decide to change LINE to be another"
   "set of X and Y coordinates, instead of a length and direction,"
   "we could modify CHANGE-LENGTH to calculate the new position."
   "Since CHANGE-LENGTH is called automatically by SET-LEN, the user"
   "code would not"
   "have to be changed. It would keep sending a message to SET-LEN"
   "with a new length and never know that we modified two variables and"
   "changed the representation of LINE. This is another powerful"
   "feature of object oriented programming, the ability to change"
   "the way data is structured and yet not have to change"
   "any code that uses the data."))

(frame
  CONCLUSION
  ("You may want to print out the file /scpsdemo.s, if you haven't already"
   "done so, and look at the definitions of the classes. In the file you"
   "will notice that the class RECTANGLE inherits POINT's"
   "variables indirectly by inheriting LINE.")
  ()
  ("Following this tutorial there is a demonstration using the class"
   "RECTANGLE. During the demonstration it is not possible to go"
   "backwards, only forwards.")
  ()
  "Conclusion"
  ("SCPSDEMO.S file"))


;
; This is an example of using SCOOPS. Please refer to chapter 5 in the
; Language Reference Manual for TI Scheme.
;
; The first thing that needs to be done is to define classes for different
; types. We will define three types, points, lines and rectangles.

;;;
;;; Point, Line and Rectangle
;;;

;;;
;;; Class POINT
;;;


(define-class point
	      (instvars (x     (active 0       () move-x))
			(y     (active 0       () move-y))
			(color (active 'yellow () change-color)))
	      (options	settable-variables
			inittable-variables))

(compile-class point)	      ; see page 45 in the language reference manual

;;;
;;; Class LINE
;;;

(define-class line
	      (instvars (len (active 50 () change-length))
			(dir (active 0	() change-direction)))
	      (mixins point)  ; inherit x, y, and color from point class.
	      (options settable-variables))

(compile-class line)

;;;
;;; Class RECTANGLE
;;;

(define-class rectangle
	      (instvars (height (active 60 () change-height)))
	      (mixins line)  ; inherit color and width (len) from line
	      (options settable-variables))

(compile-class rectangle)

; In order to have an occurance of a class you will need to use the
; MAKE-INSTANCE procedure. For example:
;     (define p1 (make-instance point))
; Then to change parts of the class use the send function. For example
; to change the color of the point previously defined:
;     (send p1 change "color" 'cyan)
;

;;;
;;; Methods for POINT
;;;

(define-method (point erase) ()
	       (set-pen-color! 'black)
	       (draw))

(define-method (point draw) ()
	       (draw-point x y))

; having both a draw and redraw function here may seem to be unnecessary.
; you will see why both are needed as we continue

(define-method (point redraw) ()
	       (set-pen-color! color)
	       (draw))

(define-method (point move-x) (new-x)
	       (erase)
	       (set! x new-x)
	       (redraw)
	       new-x)

(define-method (point move-y) (new-y)
	       (erase)
	       (set! y new-y)
	       (redraw)
	       new-y)

(define-method (point change-color) (new-color)
	       (erase)
	       (set! color new-color)
	       (redraw)
	       new-color)
;;;
;;; Methods for LINE
;;;

; inherit erase, redraw, move-x, move-y and change-color from point.

(define-method (line draw) ()
	       (position-pen x y)
	       (draw-line-to (truncate (+ x (* len (cos dir))))
			     (truncate (+ y (* len (sin dir))))))

(define-method (line change-length) (new-length)
	       (erase)
	       (set! len new-length)
	       (redraw)
	       new-length)

(define-method (line change-direction) (new-dir)
	       (erase)
	       (set! dir new-dir)
	       (redraw)
	       new-dir)

;;;
;;; Methods for RECTANGLE
;;;

; inherit erase, redraw, move-x, move-y and change-color from point.

(define-method (rectangle draw) ()
	       (position-pen x y)
	       (draw-line-to (+ x len) y)
	       (draw-line-to (+ x len) (+ y height))
	       (draw-line-to x (+ y height))
	       (draw-line-to x y))

(define-method (rectangle change-height) (new-height)
	       (erase)
	       (set! height new-height)
	       (redraw)
	       new-height)

;
;these are routines necessary for the last part of the tutorial
;

(define small
  (lambda ()
    (let ((video 3))	;this var is unused now
      (set! *user-error-handler*
	(lambda x
	  (display "There was an error.  Please try again.")
	  (reset)))
      (set-video-mode! 4)
      (window-clear 'console)
      (window-set-position! 'console 20 0)
      (window-set-size! 'console 4 80)
      (clear-graphics)
      (if (equal? pcs-machine-type 1)
	  (begin				; for TI machines
	    (position-pen -360 -138)
	    (draw-box-to 359 -90))
	  (begin				; for IBM
	    (if (equal? (get-video-mode) 6)
		(begin			; 640 x 200
		  (position-pen -320 -60)
		  (draw-line-to 319 -60))
		(begin			; 320 x 200
		  (position-pen -160 -60)
		  (draw-line-to 159 -60)))))
      video)))

(define finished
  (lambda ()
    (window-set-position! 'console 0 0)
    (window-set-size! 'console 24 80)
    (window-clear 'console)
    (clear-graphics)
    (set! *user-error-handler* nil)
    (set-video-mode! 3)
    ))

(define pause
  (lambda ()
    (write-char (integer->char 2))
    (read-char)
    (newline)))


(define demo

  (letrec ((B1 (make-instance rectangle))
	   (B2 (make-instance rectangle))
	   (L1 (make-instance line))

	   (prompt
	     (lambda (no command)
	       (princ "[")
	       (princ no)
	       (princ "] ")
	       (set! command (read))
	       (eval command (procedure-environment demo))
	       (if (equal? command (list 'finished))
		   0
		   (prompt (1+ no) command)))))

    (lambda ()

      (small)

      (writeln " To create an instance of a class")
      (writeln " use MAKE-INSTANCE. For example:")
      (display " (DEFINE B1 (MAKE-INSTANCE RECTANGLE))")
      (pause)
      (writeln " Notice that the MAKE-INSTANCE doesn't")
      (writeln " cause anything to appear on the screen.")
      (writeln " All we have done so far is to define")
      (display " the data strucure.")
      (pause)

      (writeln " To manipulate an instance we send ")
      (writeln " messages to it. For example:")
      (display " (SEND B1 SET-HEIGHT 40)")
      (pause)

      (send b1 set-height 40)

      (writeln " Now let's create another instance.")
      (display " (DEFINE B2 (MAKE-INSTANCE RECTANGLE))")
      (pause)

      (writeln " And change its x value to 100.")
      (display " (SEND B2 SET-X 100)")
      (pause)

      (send b2 set-x 100)

      (writeln " Since part of B1 was erased when we")
      (writeln " moved B2, let's redraw B1.")
      (display " (SEND B1 REDRAW)")
      (pause)

      (send b1 redraw)

      (writeln " We can also change the color")
      (writeln " of an instance.")
      (display " (SEND B1 SET-COLOR 2)")
      (pause)

      (send b1 set-color 2)

      (writeln " And change its width.")
      (display " (SEND B2 SET-LEN 20)")
      (pause)

      (send b2 set-len 20)

      (writeln " We can also make an instance of a line.")
      (display " (DEFINE L1 (MAKE-INSTANCE LINE))")
      (pause)

      (writeln " With lines we can also change")
      (writeln " directions, specified in radians.")
      (display " (SEND L1 SET-DIR (/ 3.14 4))")
      (pause)

      (send l1 set-dir (/ 3.14 4))

      (writeln " Of course we can also change the")
      (writeln " length of the line.")
      (display " (SEND L1 SET-LEN 100)")
      (pause)

      (send l1 set-len 100)

      (writeln " Now's the time for you to try sending")
      (writeln " messages on your own! You can define")
      (writeln " new instances or manipulate B1, B2 and")
      (display " L1.")
      (pause)
      (writeln " Enter (FINISHED) when you're through.")

      (let ((command '()))
	(prompt 1 command)))))

