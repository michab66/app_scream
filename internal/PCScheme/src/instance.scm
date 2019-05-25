
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                 ;;;
;;;                     S c o o p s                                 ;;;
;;;                                                                 ;;;
;;;      (c) Copyright 1985 Texas Instruments Incorporated          ;;;
;;;                  All Rights Reserved                            ;;;
;;;                                                                 ;;;
;;;               File updated : 8/28/85                            ;;;
;;;                                                                 ;;;
;;;                   File : instance.scm                           ;;;
;;;                                                                 ;;;
;;;                   Amitabh Srivastava                            ;;;
;;;                                                                 ;;;
;;;    This file contains the compiling and making of an instance.  ;;;
;;;                                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;

(macro compile-class
  (lambda (e)
    (let ((name (cadr e))
	  (class (%sc-name->class (cadr e))))
      (if (%sc-class-compiled class)
	  name
          (begin
	   (%inherit-method-vars class)
	   (%make-template name class))))))

;;;

(define %sc-compile-class
  (lambda (class)
    (%inherit-method-vars class)
    (eval (%make-template (%sc-name class) class)
          user-initial-environment)))

;;;

(macro make-instance
  (lambda (e)
    (cons (list '%sc-inst-template (cadr e)) (cddr e))))
;;;

(define %uncompiled-make-instance
  (lambda (class)
    (lambda init-msg
      (%sc-compile-class class)
      (apply (%sc-inst-template class) init-msg))))



;;;

(define %make-template
  (lambda (name class)
    `(begin
;;; do some work to make compile-file work
       (%sc-set-allcvs ,name ',(%sc-allcvs class))
       (%sc-set-allivs ,name ',(%sc-allivs class))
       (%sc-set-method-structure ,name
            ',(%sc-method-structure class))
;;; prepare make-instance template
       (%sc-set-inst-template ,name
          ,(%make-inst-template (%sc-allcvs class)
                               (%sc-allivs class)
                               (%sc-method-structure class)
                               name class))
       (%sc-set-class-compiled ,name #!TRUE)
       (%sc-set-class-inherited ,name #!TRUE)
       (%sign-on ',name ,name)
;;;
       ',name)))
;;;


(define %make-inst-template
  (lambda (cvs ivs method-structure name class)
    (let ((methods
            (append
                (mapcar
                  (lambda (a)
                    `(,(car a) (%sc-get-meth-value ',(car a) ,(caadr a))))
                  method-structure)
                 '((%*methods*% '-))))
          (classvar (append cvs '((%*classvars*% '-))))
          (instvar  (append ivs '((%*instvars*% '-)))))
      `(let ((%sc-class ,name))
         (let ,methods
           (%sc-set-method-env ,name (the-environment))
            (let ,classvar
              (%sc-set-class-env ,name (the-environment))
              (lambda %sc-init-vals
                (let ,instvar
                  (the-environment)))))))))

