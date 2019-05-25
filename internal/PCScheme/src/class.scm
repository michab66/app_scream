
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                 ;;;
;;;                     S c o o p s                                 ;;;
;;;                                                                 ;;;
;;;      (c) Copyright 1985 Texas Instruments Incorporated          ;;;
;;;                  All Rights Reserved                           ;;;
;;;                                                                 ;;;
;;;               File updated : 8/29/85                            ;;;
;;;                                                                 ;;;
;;;                   File : class.scm                               ;;;
;;;                                                                 ;;;
;;;                 Amitabh Srivastava                              ;;;
;;;                                                                 ;;;
;;;    This file contains class creation and function to access     ;;;
;;;    various fields.                                              ;;;
;;;                                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; 
(define %%class-tag '#!class)

(define %sc-make-class
  (lambda (name cv allivs mixins method-values)
    (let ((method-structure
                  (mapcar (lambda (a) (list (car a) (cons name name)))
                          method-values))
          (class (make-vector 15)))
       (vector-set! class 0 %%class-tag)
       (vector-set! class 1 name)
       (vector-set! class 2 cv)
       (vector-set! class 3 cv)
       (vector-set! class 4 allivs)
       (vector-set! class 5 mixins)
       (vector-set! class 6 (%uncompiled-make-instance class))
       (vector-set! class 9 method-structure)
       (vector-set! class 13 method-values)
       (vector-set! class 14 allivs)            
       (putprop name class '%class)
       class)))

(define %scoops-chk-class
  (lambda (class)
    (and (not (and (vector? class)
                   (> (vector-length class) 0)
                   (equal? %%class-tag (vector-ref class 0))))
         (error-handler class 6 #!TRUE))))


;;; 

(define-integrable %sc-name
  (lambda (class)
    (vector-ref class 1)))
           
;;; 

(define-integrable %sc-cv
  (lambda (class)
    (vector-ref class 2)))
           
;;; 

(define-integrable %sc-allcvs
  (lambda (class)
    (vector-ref class 3)))
           
;;; 

(define-integrable %sc-allivs
  (lambda (class)
    (vector-ref class 4)))
           
;;; 

(define-integrable %sc-mixins
  (lambda (class)
    (vector-ref class 5)))
           
;;; 

(define-integrable %sc-inst-template
  (lambda (class)
    (vector-ref class 6)))
           
;;; 

(define-integrable %sc-method-env
  (lambda (class)
    (vector-ref class 7)))
           
;;; 

(define-integrable %sc-class-env
  (lambda (class)
    (vector-ref class 8)))
           

;;; 

(define-integrable %sc-method-structure
  (lambda (class)
    (vector-ref class 9)))
           
;;; 

(define-integrable %sc-subclasses
  (lambda (class)
    (vector-ref class 10)))
           
;;; 

(define-integrable %sc-class-compiled
  (lambda (class)
    (vector-ref class 11)))
           
;;; 

(define-integrable %sc-class-inherited
  (lambda (class)
    (vector-ref class 12)))
           
;;; 

(define-integrable %sc-method-values
  (lambda (class)
    (vector-ref class 13)))

(define-integrable %sc-iv
  (lambda (class)
    (vector-ref class 14)))


;;; 

(define-integrable %sc-set-name
  (lambda (class val)
    (vector-set! class 1 val)))
           
;;; 

(define-integrable %sc-set-cv
  (lambda (class val)
    (vector-set! class 2 val)))
           

;;; 

(define-integrable %sc-set-allcvs
  (lambda (class val)
    (vector-set! class 3 val)))
           
;;; 

(define-integrable %sc-set-allivs
  (lambda (class val)
    (vector-set! class 4 val)))
           
;;; 

(define-integrable %sc-set-mixins
  (lambda (class val)
    (vector-set! class 5 val)))
           
;;; 

(define-integrable %sc-set-inst-template
  (lambda (class val)
    (vector-set! class 6 val)))
           
;;; 

(define-integrable %sc-set-method-env
  (lambda (class val)
    (vector-set! class 7 val)))
           
;;; 

(define-integrable %sc-set-class-env
  (lambda (class val)
    (vector-set! class 8 val)))
           
;;; 

(define-integrable %sc-set-method-structure
  (lambda (class val)
    (vector-set! class 9 val)))
           
;;; 

(define-integrable %sc-set-subclasses
  (lambda (class val)
    (vector-set! class 10 val)))
           

;;; 

(define-integrable %sc-set-class-compiled
  (lambda (class val)
    (vector-set! class 11 val)))
           
;;; 

(define-integrable %sc-set-class-inherited
  (lambda (class val)
    (vector-set! class 12 val)))
           
;;; 

(define-integrable %sc-set-method-values
  (lambda (class val)
    (vector-set! class 13 val)))
           
;;;

(define-integrable %sc-set-iv
  (lambda (class val)
    (vector-set! class 14 val)))


;;;

(define %sc-name->class
  (lambda (name)
    (apply-if (getprop name '%class)
              (lambda (a) a)
              (error-handler name 2 #!TRUE))))
           
;;;

(define-integrable %sc-get-meth-value
  (lambda (meth-name class)
    (cdr (assq meth-name (%sc-method-values class)))))

;;;

(define-integrable %sc-get-cv-value
  (lambda (var class)
    (cadr (assq var (%sc-cv class)))))

;;;
 
(define-integrable %sc-concat
  (lambda (str sym)
    (string->symbol (string-append str (symbol->string sym)))))
