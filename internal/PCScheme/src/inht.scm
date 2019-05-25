

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                 ;;;
;;;                     S c o o p s                                 ;;;
;;;                                                                 ;;;
;;;      (c) Copyright 1985 Texas Instruments Incorporated          ;;;
;;;                  All Rights Reserved                            ;;;
;;;                                                                 ;;;
;;;               File updated : 8/29/85                            ;;;
;;;                                                                 ;;;
;;;                   File : inht.scm                               ;;;
;;;                                                                 ;;;
;;;                 Amitabh Srivastava                              ;;;
;;;                                                                 ;;;
;;;    This file contains the inheritance details.                  ;;;
;;;                                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;

(define %inherit-method-vars
  (lambda (class)
    (or (%sc-class-inherited class)
	(%inherit-from-mixins
	 (%sc-allcvs class)
	 (%sc-allivs class)
	 (%sc-method-structure class)
	 (%sc-mixins class)
	 class
	 (lambda (class cvs ivs methods)
	   (%sc-set-allcvs class cvs)
	   (%sc-set-allivs class ivs)
	   (%sc-set-method-structure class methods)
           (%sc-set-class-inherited class #!true)
           (%sign-on (%sc-name class) class)
	   class)))))

;;;

(define %sign-on
  (lambda (name class)
    (mapcar 
      (lambda (mixin)
        (let* ((mixin-class (%sc-name->class mixin))
               (subc (%sc-subclasses mixin-class)))
          (if (not (%sc-class-inherited mixin-class))
              (%inherit-method-vars mixin-class))
          (or (memq name subc)
              (%sc-set-subclasses mixin-class (cons name subc)))))
      (%sc-mixins class))))



;;;

(define %inherit-from-mixins
  (letrec
    ((insert-entry
      (lambda (entry class1 method-entry name2 previous current)
        (cond ((null? current) 
               (set-cdr! previous
                         (cons (cons (caadr method-entry) name2) '())))
              ((%before name2 (cdar current) (%sc-name class1))
               (set-cdr! previous
                         (cons (cons (caadr method-entry) name2) current)))
              (else '()))))

    (insert
      (lambda (struct1 entry class1 struct2 name2)
        ((rec loop-insert
           (lambda (struct1 entry class1 struct2 name2 previous current)
             (if (insert-entry entry class1 struct2 name2 previous current)
                 struct1
                 (loop-insert struct1 entry class1 struct2 name2
                              current (cdr current)))))
         struct1 entry class1 struct2 name2 entry (cdr entry))))
                 
    (add-entry
      (lambda (struct1 class1 method-entry name2)
        (cons (list (car method-entry) (cons (caadr method-entry) name2))
              struct1)))

    (combine-methods 
      (lambda (struct1 class1 struct2 name2)
        ((rec loop-combine
           (lambda (struct1 class1 struct2 name2)
             (if struct2
                 (loop-combine
                   (let ((entry (assq (caar struct2) struct1)))
                      (if entry
                          (insert struct1 entry class1 (car struct2) name2)
                          (add-entry struct1 class1 (car struct2) name2)))
                   class1 
                   (cdr struct2)
                   name2)
                 struct1)))
         struct1 class1 struct2 name2)))

     (combine-vars
       (lambda (list1 list2)
         ((rec loop-combine 
            (lambda (list1 list2)
              (if list2
                  (loop-combine
                    (if (assq (caar list2) list1)
                        list1
                        (cons (car list2) list1))
                    (cdr list2))
                  list1)))
          list1 list2)))

     )    

  (lambda (cvs ivs methods mixins class receiver)
    ((rec loop-mixins
       (lambda (cvs ivs methods mixins class receiver)
         (if mixins 
             (let ((mixin-class (%sc-name->class (car mixins))))
               (%inherit-method-vars mixin-class)
               (loop-mixins
                 (combine-vars cvs (%sc-allcvs mixin-class))
                 (combine-vars ivs (%sc-allivs mixin-class))
                 (combine-methods methods class
                          (%sc-method-structure mixin-class) (car mixins))
                 (cdr mixins)
                 class
                 receiver))
             (receiver class cvs ivs methods ))))
     cvs ivs methods mixins class receiver))))

