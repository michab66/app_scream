

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                 ;;;
;;;                     S c o o p s                                 ;;;
;;;                                                                 ;;;
;;;      (c) Copyright 1985 Texas Instruments Incorporated          ;;;
;;;                  All Rights Reserved                            ;;;
;;;                                                                 ;;;
;;;               File updated : 8/29/85                            ;;;
;;;                                                                 ;;;
;;;                   File : utl.scm                                ;;;
;;;                                                                 ;;;
;;;                 Amitabh Srivastava                              ;;;
;;;                                                                 ;;;
;;;    This file contains various utility routines                  ;;;
;;;                                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;   Error handler. Looks up the error message in the table and
;;;   prints it.

    (define error-handler
      (let ((error-table
        (let ((table (make-vector 8)))
          (vector-set! table 0 " Invalid class definition ")
	  (vector-set! table 1 " Invalid option ")
	  (vector-set! table 2 " Class not defined ")
	  (vector-set! table 3 " Method has been deleted ")
	  (vector-set! table 4 " Method is not present ")
	  (vector-set! table 5 " Variable is not present")
          (vector-set! table 6 " Not a Scoops Class")
          (vector-set! table 7 " Class not compiled ")
	  table)))
	(lambda (msg number flag)
	  (if flag
	      (error (vector-ref error-table number) msg)
	      (bkpt  (vector-ref error-table number) msg)))))


;;;   some functions defined globally which will be moved locally later

        (define %sc-class-description
           (lambda (class)
              (writeln " ")
              (writeln "    CLASS DESCRIPTION    ")
              (writeln "    ==================    ")
              (writeln " ")
              (writeln " NAME            : " (%sc-name class))
              (writeln " CLASS VARS      : "
                       (mapcar car (%sc-allcvs class)))
              (writeln " INSTANCE VARS   : "
                       (mapcar car (%sc-allivs class)))
              (writeln " METHODS         : "
                       (mapcar car (%sc-method-structure class)))
              (writeln " MIXINS          : " (%sc-mixins class))
              (writeln " CLASS COMPILED  : " (%sc-class-compiled class))
              (writeln " CLASS INHERITED : " (%sc-class-inherited class))
           ))
;;;

    (define %sc-inst-desc
       (lambda (inst)
         (letrec ((class (access %sc-class inst))
                  (printvars
                    (lambda (f1 f2)
		      (if f1
			  (begin
			   (writeln "   " (caar f1) " : "
				    (cdr (assq (caar f1) f2)))
			   (printvars (cdr f1) f2))))))
            (writeln " ")
	    (writeln "  INSTANCE DESCRIPTION      ")
	    (writeln "  ====================      ")
	    (writeln " ")
	    (writeln " Instance of Class " (%sc-name class))
	    (writeln " ")
	    (writeln " Class Variables : ")
            (printvars (%sc-allcvs class)
		       (environment-bindings (%sc-class-env class)))
            (writeln " ")
            (writeln "Instance Variables :")
            (printvars (%sc-allivs class) (environment-bindings inst))
           )))
;;;

(define describe
  (lambda (class-inst)
    (if (vector? class-inst)
        (begin
          (%scoops-chk-class class-inst)
          (%sc-class-description class-inst))
        (%sc-inst-desc class-inst))))


(define %scoops-chk-class-compiled
  (lambda (name class)
    (or (%sc-class-compiled class)
        (error-handler name 7 #!true))))

;;; (rename-class (class new-name))

(macro rename-class
  (lambda (e)
    (let ((class (caadr e))
          (new-name (cadadr e)))
      `(begin
         (%sc-name->class ',class)
         (%sc-set-name ,class ',new-name)
         (set! (access ,new-name user-initial-environment) ,class)
         (putprop ',new-name ,new-name '%class)
         ',new-name))))

;;; (getcv class var)

(macro getcv
  (lambda (e)
    (let ((class (cadr e))
          (var (caddr e)))
      `(begin
         (and (%sc-name->class ',class)
              (%scoops-chk-class-compiled ',class ,class))
         (send (%sc-class-env ,class) ,(%sc-concat "GET-" var))))))

;;; (setcv class var val)

(macro setcv
  (lambda (e)
    (let ((class (cadr e))
          (var (caddr e))
          (val (cadddr e)))
      `(begin
         (and (%sc-name->class ',class)
              (%scoops-chk-class-compiled ',class ,class))
         (send (%sc-class-env ,class) ,(%sc-concat "SET-" var) ,val)))))

;;; (class-compiled? class)

(define class-compiled?
  (lambda (class)
    (%scoops-chk-class class)
    (%sc-class-compiled class)))


;;;  (class-of-object object)

(define class-of-object
  (lambda (obj)
    (%sc-name (access %sc-class obj))))

;;; (name->class name)

(define name->class
  (lambda (name)
    (%sc-name->class name)))

;;;

(define %sc-class-info
  (lambda (fn)
    (lambda (class)
      (%scoops-chk-class class)
      (mapcar car (fn class)))))

;;;

(define methods (%sc-class-info %sc-method-values))

;;;

(define all-methods (%sc-class-info %sc-method-structure))

;;;

(define classvars (%sc-class-info %sc-cv))

;;;

(define all-classvars (%sc-class-info %sc-allcvs))

;;;

(define instvars (%sc-class-info %sc-iv))

;;;

(define all-instvars (%sc-class-info %sc-allivs))


;;;

(define mixins
  (lambda (class)
    (%scoops-chk-class class)
    (%sc-mixins class)))