
;(load "scoops.fsl")

(define extensions
  (let ((blanks (make-string 10 #\space)))
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
  ("The tutorial will follow these conventions:"
	"-- words in /yellow introduce new terms"
;	 "-- words in @red mark notable points"
	"-- a happy-face character, like you see"
	"at the bottom of this screen,"
	"means Scheme is waiting on you to press a key"
	"-- and text in green is the item under discussion.")
  (:data "The SCOOPS Tutorial" :pp-data))

(frame
  ()
  ("This tutorial starts with an introduction to"
   "object-oriented programming. Then it takes you through"
   "SCOOPS, showing how to code with it and introducing"
   "design principles appropriate to an object-oriented system."
   "When the"
   "tutorial is finished, you will have an opportunity to try your"
   "own hand at working with SCOOPS."
;   "The classes for this tutorial are POINT, LINE and RECTANGLE."
   )
  ()
  ("Chapter 5 in the TI Scheme Language Reference Manual contains"
   "full information on SCOOPS."))

(frame
  SCOOPS
  ("/SCOOPS stands for the /SCheme /Object-Oriented /Programming /System."
   "Object-oriented programming deals with the"
   "interactions of /objects, which have properties"
   "of data and code combined."
   "An object consists of variables, which are private to the object"
   "and contain its local state,"
   "and procedures, also private to the object, which define its behavior.")
  ()
  ()
  ()
  "Introduction to Object-Oriented Programming"
  ("object" "SCOOPS"))

(frame
  ()
  ("You communicate with an object by sending it a /message, which"
   "is somewhat like a procedure call."
   "The object responds by executing one of its"
   "local procedures and returning a value to"
   "the caller.")
  ()
  ("Unlike conventional languages where the caller directly invokes"
   "a procedure by using its name, the object approach allows the"
   "object to substitute any procedure it sees fit that can perform"
   "the task that the message names--the caller"
   "cannot force it to call a specific procedure.")
  ()
  ()
  ("message"))

(frame
  ()
  ("What we've really done is swap the roles of who's controlling who."
   "In conventional languages, the caller has control over"
   "what procedures get executed with what data."
   "In the object-oriented approach, the data decides"
   "what procedures it will use on itself, and uses the message mechanism"
   "to keep the caller from knowing the details about how it"
   "did it.")
  ()
  ("This is the fundamental point of a message."
   "It requests some kind of action on the object's part, but"
   "the wording of the message implies nothing about how it"
   "performs the action. This gives an object great flexibility"
   "in how it is implemented, and it greatly enhances the"
   "modularity of the system.")
  ()
  ()
  ("message"))

(frame
  ()
  ("How does one build an object?"
   "First you declare the kind of object you want;"
   "this is called the object's /class."
   "Defining a class is somewhat like defining a \"record\" or \"structure\""
   "in a conventional language--it declares what an object looks like,"
   "but it doesn't actually create one."
   "A class definition will include things like:"
   "-- the name of the class"
   "-- what variables are local to the object and what ones can it reference"
   "-- what are their default values"
   "-- declarations of simple local procedures"
   "-- the inheritance structure of the class."
   "This last is a very powerful feature of an object-oriented system,"
   "and we will come back to it later.")
  ()
  ()
  ()
  ()
  ("class"))

(frame
  ()
  ("Now, using the class description, we can create as many objects"
   "of that kind as we wish."
   "A created object is called an /instance."
   "It takes up memory space, keeps its own state, and knows"
   "what kind of object it is (its class)."
   "In a conventional language, this is akin to dynamically allocating space"
   "for a \"record\" variable.")
  ()
  ("Incidentally, the variables local to each instance are called"
   "/instance /variables."
   "One way to distinguish one instance from another is to look"
   "at the state information contained in the instance variables."
   "That is, of course, if the object will let us look at them.")
  ()
  ()
  ("instance" "instance variable"))

(frame
  ()
  ("Some object-oriented systems allow only instance variables;"
   "that is, an instance cannot refer to any other variables than its own."
   "Other systems allow varying degrees of freedom in where an instance"
   "can look."
   "For example, SCOOPS allows /class /variables."
   "These are variables maintained by the class itself rather than"
   "by each instance."
   "Data that would be common to every instance can be factored out"
   "into the class and stored just once, rather than repeating it"
   "in every instance."
   "It also provides a way for an instance to transfer data to other"
   "similar instances.")
  ()
  ()
  ()
  ()
  ("class variable" ))

(frame
  ()
  ("So far what we've said makes objects sound very much like"
   "conventional data structures such as records."
   "But objects are not just places that hold variables--they hold"
   "code too."
   "These are the local procedures, or /methods, of an object."
   "They're called methods to distinguish them from procedures"
   "in a conventional language, which can be directly invoked by name."
   "Methods cannot be directly invoked."
   "Instead, an object, upon getting a message, decides what method"
   "gets invoked."
   "This determination can get quite involved, as you will see;"
   "a conventional language has nothing quite like it.")
  ()
  ()
  ()
  ()
  ("method"))

(frame
  ()
  ("Unlike instance variables, which must all be declared in"
   "a class declaration, so that every instance gets the same set,"
   "methods can be added anytime to a class."
   "Methods are local to a class, really, rather than each instance,"
   "and so the code exists in the class, avoiding duplication for"
   "each instance."
   "More importantly, when a method is added or altered, all instances"
   "\"see\" the effects immediately.")
  ()
  ()  ;slot for text below
;  ("For the record, to show some of the possibilities,"
;   "some object-oriented systems"
;   "actually do allow \"instance methods\"."
;   "The \"ultimate\" architecture for an object-oriented programming"
;   "system has yet to be determined.")
  ()
  ()
  ("method"))

(frame
  ()
  ("You've probably noticed that sometimes we're a little sloppy"
   "in our terminology, for example, confusing \"instance\" and \"object\"."
   "An \"object\" is really an abstract entity while an \"instance\""
   "is its realization on a computer, but in practice everyone knows"
   "what you mean."
   "The words \"program\" and \"algorithm\" are also often used"
   "interchangeably; it's no different here.")
  ()
  ("Similarly, we've talked about instances having \"instance variables\""
   "and \"methods\"."
   "Looked at abstractly, an instance does have these properties."
   "On a real machine, though, it'd be real expensive if each instance"
   "had to have its own copy of its methods, and so the implementation"
   "collects them in the class to save space."
   "But that is the implementor's worry and shouldn't be yours."
   "The whole idea of objects is that you can't peer inside one."
   "If you could seriously take advantage of what you might find there,"
   "then it only indicates that the implementation must leave"
   "something to be desired.")
  ()
  ()
  ("instance" "object" "method" "abstraction vs. implementation"))

(frame
  ()
  ("Now that you've got the general idea, let's get acquainted with SCOOPS"
   "so that you can see objects in action and how you go about"
   "constructing a program with objects."))

(frame
  CLASS
  ("Objects often model real-world items."
   "For the tutorial, we will construct a \"graphics world\""
   "populated with shapes like points, lines, and rectangles."
   "We will model the kinds of shapes with classes, and"
   "individual shapes will be instances of those classes."
   "To start out, then, we use the special form /DEFINE-CLASS"
   "to create a class."
   "For example:")
  (:eval (or (getprop 'define-class 'pcs*macro)
	     (load "scoops.fsl"))
   :data (define-class point (instvars (x 0) (y 0)))
   :data-eval :pp-data)
  ("This defines a class named \"POINT\". Each instance of the class"
   "contains two instance variables called X and Y, and each"
   "is initialized to zero."
   "! !This form of the definition allows us to create instances,"
   "but that's all."
   "We cannot create them with different initial values,"
   "and once created, we cannot look at or change them in any way.")
  ()
  "Defining a Class"
  ("DEFINE-CLASS"))

(frame
  ()
  ("Let's explore some of the possibilities of DEFINE-CLASS."
   "First let's change POINT's definition so that we can create"
   "points whose X and Y values won't always be zero."
   "It would be rather dull to have all our points"
   "be synonymous with the origin.")
  (:data (define-class point (instvars (x 0) (y 0))
     (options inittable-variables)) :data-eval :pp-data)
  ("The /inittable-variables option allows us to override"
   "the default instance variable values whenever we create a point."
   "The form used allows all of them to be initialized."
   "If there is some reason to restrict which variables can be"
   "initialized, instead of saying \"inittable variables\""
   "we could have said something like \"(inittable variables x)\","
   "which would allow only X to be initialized--Y would always get"
   "its default value.")
  ()
  ()
  ("inittable-variables" "class options"))

(frame
  ()
  ("So far we can create points,"
   "but we can't do anything with the X,Y values."
   "They are inside a point object and are hidden to any caller."
   "To do anything further requires object-local procedures--methods--to"
   "handle the point's representation for the outside world."
   "! !The simplest methods are those that just retrieve instance"
   "variable values and return them. They are so simple, in fact,"
   "that SCOOPS can create them on its own.")
  (:data (define-class point (instvars (x 0) (y 0))
     (options  gettable-variables inittable-variables)) :data-eval :pp-data)
  ("We have a new option, /gettable-variables."
   "This creates two messages, GET-X and GET-Y;"
   "two methods, one to retrieve X and one to retrieve Y,"
   "and associates the methods with the messages."
   "As before, we can restrict which instance variables"
   "get the gettable methods.")
  ()
  ()
  ("gettable-variables" "class options"))

(frame
  ()
  ("For the final touch, let's be able to change the instance variables."
   "We have a new option.")
  (:data (define-class point (instvars (x 0) (y 0))
     (options  settable-variables
	       gettable-variables
	       inittable-variables)) :data-eval :pp-data)
  ("The /settable-variables option creates the messages SET-X and SET-Y,"
   "methods to change X and Y, and associates them."
   "Here, too, we can restrict which instance variables are settable.")
  ()
  ()
  ("settable-variables" "class options"))

(frame
  ()
  ("You should note that the different options are mutually exclusive."
   "You can have settable but not gettable variables, settables but"
   "not inittables, or any other combination. And if you have no options"
   "you get immutable objects that can't be initialized, examined, or"
   "altered.")
  ()
  ("We should emphasize that"
   "the notions of \"gettable\", \"settable\", and \"inittable\" are all"
   "relative to what a caller sees."
   "To the object's own methods, the instance variables are always"
   "accessible at all times with no restrictions."
   "This will be clearer when we define a method ourselves rather"
   "than letting the system do it.")
  ()
  ()
  ("settable-variables" "gettable-variables" "inittable-variables" 
                       "class options"))

(frame
  ()
  ("Although we haven't exhausted all the features of DEFINE-CLASS,"
   "let's move on."
   "We'll explore more of them as we need them."))

(frame
  ()
  ("Let's detour for a moment."
   "We've being redefining our class so much, what does it really"
   "look like at this moment?"
   "Using /(DESCRIBE class-name) will tell us."
   "Here's what it says for POINT.")
  (:output (DESCRIBE POINT))
  ("The class name, instance variables, and methods we have already described;"
   "the other items we have yet to discuss."
   "DESCRIBE doesn't indicate which variables are initializable,"
   "nor which methods are automatically generated.")
  ()
  ()
  ("DESCRIBE"))

(frame
  COMPILE-CLASS
  ("Before we can create instances, we need to /compile the class."
   "We don't mean here that we generate code for the class,"
   "but rather we (re)organize the class's inheritance structure"
   "for efficient execution."
   "This can take time, depending on the complexity of the"
   "inheritance structure."
   "We don't have to do anything special, since the class"
   "can be compiled when its first instance is made."
   "Often, though, we know the class's complete inheritance structure"
   "at compile (as in \"generate code\") time."
   "If so, we can let the compiler compile the class."
   "Then the system needn't do it while executing the program."
   "We can use the special forms COMPILE-CLASS to compile the class"
   "and CLASS-COMPILED? to see whether it has been or not.")
  (:data (COMPILE-CLASS POINT) :data-eval :pp-data
   :eval (fresh-line)
   :data (CLASS-COMPILED? POINT) :data-eval :pp-data :yields :pp-evaled-data)
  ()
  ()
  ()
  ("compiling a class" "COMPILE-CLASS" "CLASS-COMPILED?"))

(frame
  MAKE-INSTANCE
  ("To create an instance of a class you use the special form"
   "MAKE-INSTANCE. The simplest format of it is:")
  (:data (DEFINE P1 (MAKE-INSTANCE POINT)) :data-eval :pp-data)
  ("We've created a POINT instance and assigned it to variable P1."
   "Since we said nothing special about initializing anything,"
   "X and Y get their default values of zero.")
  ()
  "Defining Instances"
  ("MAKE-INSTANCE" "creating instances"))

(frame
  ()
  ("To verify what we just said, we can DESCRIBE an instance as well"
   "as a class."
   "\"(DESCRIBE P1)\" gives this output:")
  (:output (DESCRIBE P1))
  ("This tells us which class the object is an instance of"
   "and the values of all the variables it can access.")
  ()
  ()
  ("DESCRIBE"))

(frame
  ()
  ("We can create arbitrary points by initializing them with"
   "appropriate X and Y values.")
  (:data (define p2 (make-instance point 'y 10)) :data-eval :pp-data
   :eval (fresh-line)      
   :data (define p3 (make-instance point 'x 5 'y 15)) :data-eval :pp-data)
  ("For point P2 we initialized Y but let X default to zero."
   "For point P3 we initialized everything."
   "! !The next frame has DESCRIBE's of P2 and P3.")
  ()
  ()
  ("MAKE-INSTANCE"))

(frame
  ()
  ()
  (:output (describe p2) :output (describe p3)))

(frame
  SEND
  ("In order to change the values of X and Y we would send a message to P1"
   "specifying the method we want to use to manipulate the data. For example,"
   "the command:")
  (:data (SEND P1 SET-X 50) :data-eval :pp-data)
  ("would change the value of X from 0, the initial value, to"
   "50.")
  ()
  "Sending Messages")

(frame
  DEFINE-METHOD
  ("To define a method for a class you would use the special form"
   "/DEFINE-METHOD. Let's define a method to display the instances of"
   "the point class we've created. For example:")
  (:data (DEFINE-METHOD (POINT DRAW) () (DRAW-POINT X Y)) :data-eval :pp-data)
  ("What we would have to do now is to send two messages, one"
   "to change the value of X or Y and another to draw the point."
   "This would be fine if we only wanted to put points on the"
   "screen that were the same color and didn't mind old occurances"
   "hanging around.")
  ()
  "Defining Methods")

(frame
  ()
  ("First we can modify the class definition to include color. This is"
   "simply adding another instance variable to be used to define the"
   "color. Our class \"POINT\" could now be defined as:")
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
   "-\"(ACTIVE 'INITIAL-VALUE' 'GET-FN' 'SET-FN')\" is used. Now when"
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
  "Active Values")

(frame
  ()
  ("Now when we send a message to P1 to set X to some"
   "value, the procedure MOVE-X is called automatically."
   "Of course we still need to"
   "write the procedures MOVE-X, MOVE-Y and CHANGE-COLOR.")
  (:data (compile-class point) :data-eval))


(frame
  ()
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
   "-We could, if we wanted, send a message to P1 and have the"
   "X value changed two ways. Either you can send a message to the"
   "MOVE-X method with a new value to which to set the variable or you"
   "can send a message to the SET-X method with a value and let Scheme"
   "call the MOVE-X method automatically.")
  (:data (define p1 (make-instance point)) :data-eval
   :data (send p1 move-y -50) :data-eval :pp-data :fresh-line
   :data (send p1 set-y -50) :data-eval :pp-data
   :data (send p1 erase) :data-eval)
  ("These two calls are equivalent since SET-Y will automatically call"
   "MOVE-Y."))

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
  "Inheritance")

(frame
  ()
  ("In addition to inheriting variables from other classes, methods"
   "are also inherited. This means that we do not have to define an"
   "erase method, we inherited it from \"POINT\". In fact the only methods"
   "we have to define are CHANGE-LENGTH, CHANGE-HEIGHT and DRAW."
   "We need our own draw method to draw a line instead of a point."
   "The practice of writing your methods to be as general as"
   "possible facilitates the inheritance feature."))

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
   "changed the representation of line. This is another powerful"
   "feature of object oriented programming, the ability to change"
   "the way the data is structured and not have to change the user"
   "program!"))

(frame
  CONCLUSION
  ("You may want to print out the file scpsdemo.s, if you haven't already"
   "done so, and look at the definitions of the classes. You"
   "will notice that the class \"RECTANGLE\" inherits \"POINT's\""
   "variables indirectly by inheriting \"LINE\".")
  ()
  ("Following this tutorial there is a demonstration using the class"
   "\"RECTANGLE\". During the demonstration it is not possible to go"
   "backwards, only forwards. A light touch on the keyboard is advised.")
  ()
  "Conclusion")

