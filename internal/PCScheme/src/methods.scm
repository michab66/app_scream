


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                 ;;;
;;;                     S c o o p s                                 ;;;
;;;                                                                 ;;;
;;;      (c) Copyright 1985 Texas Instruments Incorporated          ;;;
;;;                  All Rights Reserved                            ;;;
;;;                                                                 ;;;
;;;               File updated : 8/29/85                            ;;;
;;;                                                                 ;;;
;;;                   File : methods.scm                            ;;;
;;;                                                                 ;;;
;;;                 Amitabh Srivastava                              ;;;
;;;                                                                 ;;;
;;;    This file contains the adding of methods to classes          ;;;
;;;                                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; is class1 before class2 in class ?
;;; class1  is not equal to class2


(define %before
  (lambda (class1 class2 class)
    (or (eq? class1 class)
        (memq class2 (memq class1 (%sc-mixins (%sc-name->class class)))))))

;;;

(macro define-method
  (lambda (e)
    (let ((class-name (caadr e))
          (method-name (cadr (cadr e)))
          (formal-list (caddr e))
          (body (cdddr e)))
       (list '%sc-class-add-method
             (list 'quote class-name)
             (list 'quote method-name)
             (list 'quote class-name)
             (list 'quote class-name)
             (%sc-expand
                 (cons 'lambda (cons formal-list body)))
            (list 'lambda '(env val)
                  (list 'set! (list 'access method-name 'env) 'val))))))



;;;

(define %sc-class-add-method
  (lambda (class-name method-name method-class mixin-class method assigner)
    (let ((class (%sc-name->class class-name)))
         (apply-if (assq method-name (%sc-method-values class))
            (lambda (entry)
              (set-cdr! entry method))
            (%sc-set-method-values class
               (cons (cons method-name method) (%sc-method-values class)))))
    (%compiled-add-method class-name method-name method-class mixin-class
                         method assigner)))


;;;

(define %inform-subclasses
  (lambda (class-name method-name method-class mixin-class method assigner)
    ((rec loop
       (lambda (class-name method-name method-class mixin-class
                                       method assigner subclass)
         (if subclass
             (begin
                (%compiled-add-method
                  (car subclass) method-name method-class class-name
                  method assigner)
                (loop class-name method-name method-class mixin-class
                      method assigner
                      (cdr subclass))))))
     class-name method-name method-class mixin-class method assigner
     (%sc-subclasses (%sc-name->class class-name)))))


;;;

(define %compiled-add-method
  (lambda (class-name method-name method-class mixin-class method assigner)
    (letrec
      ((class (%sc-name->class class-name))

       (insert-entry
         (lambda (previous current)
           (cond ((null? current)
                  (set-cdr! previous
                     (cons (cons method-class mixin-class) '())))
                 ((eq? mixin-class (cdar current))
                  (set-car! (car current) method-class))
                 ((%before mixin-class (cdar current)
                           class-name)
                  (set-cdr! previous
                     (cons (cons method-class mixin-class) current)))
                 (else '()))))


       (loop-insert
         (lambda (previous current)
           (if (not (insert-entry previous current))
               (loop-insert (current) (cdr current)))))

       (insert
         (lambda (entry)
           (if (insert-entry entry (cdr entry))  ;;; insert at head
               (add-to-environment)
               (loop-insert (cdr entry) (cddr entry)))))

       (add-to-environment
         (lambda ()
           (if (%sc-class-compiled class)
               (assigner (%sc-method-env class) method))
           (if (%sc-subclasses class)
               (%inform-subclasses class-name method-name method-class
                                  mixin-class method assigner))))

       (add-entry
         (lambda ()
           (%sc-set-method-structure class
             (cons (list method-name (cons method-class mixin-class))
                   (%sc-method-structure class)))
           (add-to-environment)))
      )

      (let ((method-entry (assq method-name (%sc-method-structure class))))
        (if method-entry
            (insert method-entry)
            (add-entry))
        method-name))))
