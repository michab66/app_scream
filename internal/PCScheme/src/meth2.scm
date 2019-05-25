
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                 ;;;
;;;                     S c o o p s                                 ;;;
;;;                                                                 ;;;
;;;      (c) Copyright 1985 Texas Instruments Incorporated          ;;;
;;;                  All Rights Reserved                            ;;;
;;;                                                                 ;;;
;;;               File updated : 8/29/85                            ;;;
;;;                                                                 ;;;
;;;                   File : meth2.scm                              ;;;
;;;                                                                 ;;;
;;;                 Amitabh Srivastava                              ;;;
;;;                                                                 ;;;
;;;    This file contains the deleteion of methods from classes.    ;;;
;;;                                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;

(macro delete-method
  (lambda (e)
    (let ((class-name (caadr e))
          (method-name (cadr (cadr e))))
       (list '%sc-class-del-method
             (list 'quote class-name)
             (list 'quote method-name)
             (list 'quote class-name)
             (list 'quote class-name)
             (list 'lambda '(env val)
                   (list 'set! (list 'access method-name 'env) 'val))
             (list 'quote '())))))


;;;

(define %deleted-method
  (lambda (name)
    (lambda args
      (error-handler name 3 #!TRUE))))


;;;

(define %sc-class-del-method
  (lambda (class-name method-name method-class mixin-class assigner del-value)
    (let ((class (%sc-name->class class-name)))
      (apply-if (assq method-name (%sc-method-values class))
        (lambda (entry)
          (%sc-set-method-values class
               (delq! entry (%sc-method-values class)))
          (%compiled-del-method class-name method-name method-class mixin-class
                               assigner del-value))

        (error-handler method-name 4 #!TRUE)))))


;;;

(define %inform-del-subclasses
  (lambda (class-name method-name method-class mixin-class assigner del-value)
    ((rec loop
       (lambda (class-name method-name method-class mixin-class assigner
                del-value subclass)
         (if subclass
             (begin
                (%compiled-del-method (car subclass) method-name
                          method-class class-name assigner del-value)
                (loop class-name method-name method-class mixin-class assigner
                      del-value (cdr subclass))))))
     class-name method-name method-class mixin-class assigner del-value
     (%sc-subclasses (%sc-name->class class-name)))))


;;;

(define %compiled-del-method
  (lambda (class-name method-name method-class mixin-class assigner del-value)
    (let ((class (%sc-name->class class-name)))
      (letrec
        ((delete-entry
           (lambda (previous current)
             (cond ((eq? mixin-class (cdar current))
                    (set-cdr! previous (cdr current)) #!TRUE)
                   (else #!FALSE))))

         (loop-delete
           (lambda (previous current)
             (cond ((or (null? current)
                        (%before mixin-class (cdar previous)
                                 class-name))
                    (error-handler method-name 4 #!TRUE))
                   ((delete-entry previous current) #!TRUE)
                   (else (loop-delete current (cdr current))))))

         (delete
           (lambda (entry)
             (if (delete-entry entry (cdr entry))  ;;; delete at head
                 (modify-environment entry)
                 (loop-delete (cdr entry) (cddr entry)))))

       (modify-environment
         (lambda (entry)
	   (cond ((null? (cdr entry))
		  (%sc-set-method-structure class
		    (delq! (assq method-name (%sc-method-structure class))
			   (%sc-method-structure class)))
                  (if (%sc-class-compiled class)
                      (assigner (%sc-method-env class)
                                (or del-value
                                    (set! del-value
                                          (%deleted-method method-name)))))
		  (if (%sc-subclasses class)
		      (%inform-del-subclasses class-name method-name
			       method-class mixin-class assigner del-value)))
		 (else
		  (let ((meth-value
			 (%sc-get-meth-value method-name
					     (%sc-name->class (caadr entry)))))
		    (if (%sc-class-compiled class)
			(assigner (%sc-method-env class) meth-value))
		    (if (%sc-subclasses class)
			(%inform-subclasses class-name
					    method-name
					    method-class
					    mixin-class
					    meth-value assigner)))))))
      )

      (let ((method-entry (assq method-name (%sc-method-structure class))))
        (if method-entry
            (delete method-entry)
            (error-handler method-name 4 #!TRUE))
        method-name)))))




