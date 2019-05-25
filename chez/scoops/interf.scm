

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                 ;;;
;;;                     S c o o p s                                 ;;;
;;;                                                                 ;;;
;;;      (c) Copyright 1985 Texas Instruments Incorporated          ;;;
;;;                  All Rights Reserved                            ;;;
;;;                                                                 ;;;
;;;               File updated : 8/22/85                            ;;;
;;;                                                                 ;;;
;;;                   File : interf.scm                             ;;;
;;;                                                                 ;;;
;;;                 Amitabh Srivastava                              ;;;
;;;                                                                 ;;;
;;;    This file contains class definition and processing of        ;;;
;;;    define-class.                                                ;;;
;;;                                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;

(macro define-class
  (lambda (e)
    (let ((name (cadr e))(classvars '()) (instvars '()) (mixins '())
          (options '())(allvars '())(method-values '())(inits '()))
      (letrec
        ((chk-class-def
           (lambda (classdef)
             ((rec loop
                (lambda (deflist)
                  (if deflist
                      (begin
                        (cond ((eq? (caar deflist) 'classvars)
                               (set! classvars (cdar deflist)))
                              ((eq? (caar deflist) 'instvars)
                               (set! instvars (cdar deflist)))
                              ((eq? (caar deflist) 'mixins)
                               (set! mixins (cdar deflist)))
                              ((eq? (caar deflist) 'options)
                               (set! options (cdar deflist)))
                              (else (error-handler (caar classdef) 0 '())))
                      (loop (cdr deflist))))))
              classdef)
              (set! allvars
                    (append (mapcar (lambda (a) (if (atom? a) a (car a)))
                                     classvars)
                            (mapcar (lambda (a) (if (atom? a) a (car a)))
                                     instvars)))))


         (chk-option
           (lambda (opt-list)
             ((rec loop
                (lambda (opl meths)
                  (if opl
                      (loop
                        (cdr opl)
                        (cond ((eq? (caar opl) 'gettable-variables)
                               (append (generate-get (cdar opl)) meths))
                              ((eq? (caar opl) 'settable-variables)
                               (append (generate-set (cdar opl)) meths))
                              ((eq? (caar opl) 'inittable-variables)
                               (set! inits (cdar opl)) meths)
                              (else (error-handler (car opl) 1 '()))))
                       meths)))
              opt-list '())))

       (chk-cvs
         (lambda (list-var)
           (mapcar
             (lambda (a)
               (if (atom? a)
                   (list a '#!unassigned)
                   a))
             list-var)))

       (chk-init
         (lambda (v-form)
           (if (memq (car v-form) inits)
               (list (car v-form)
                     (list 'apply-if
                           (list 'memq
                                 (list 'quote (car v-form)) '%sc-init-vals)
                           '(lambda (a) (cadr a))
                           (cadr v-form)))
               v-form)))

       (chk-ivs
         (lambda (list-var)
           (mapcar
             (lambda (var)
               (chk-init
                  (cond ((atom? var) (list var '#!unassigned))
                        ((not-active? (cadr var)) var)
                        (else (active-val (car var) (cadr var))))))
             list-var)))

       (not-active?
         (lambda (a)
           (or (atom? a)
               (not (eq? (car a) 'active)))))

       (empty-slot?
         (lambda (form)
           (or (not form)
               (and (eq? 'nil form)
                    pcs-integrate-t-and-nil))))

       (active-val
         (lambda (var active-form)
           ((rec loop
              (lambda (var active-form getfns setfns)
                 (if (not-active? (cadr active-form))
                     (create-active
                        var
                        (if (empty-slot? (caddr active-form))
                            getfns
                            (cons (caddr active-form) getfns))
                        (list 'set! var
                              (if (empty-slot? (cadddr active-form))
                                  setfns
                                  (list (cadddr active-form) setfns)))
                        (cadr active-form))
                      (loop
                        var
                        (cadr active-form)
                        (if (empty-slot? (caddr active-form))
                            getfns
                            (cons (caddr active-form) getfns))
                        (if (empty-slot? (cadddr active-form))
                            setfns
                            (list (cadddr active-form) setfns))))))
            var active-form '() '%sc-val)))

       (create-active
         (lambda (var getfns setfns localstate)
          (set! method-values
           (cons (list 'cons
                       (list 'quote (concat "GET-" var))
                       (%sc-expand
                           (list 'lambda '() (expand-getfns var getfns))))
                 (cons (list 'cons
                             (list 'quote (concat "SET-" var))
                             (%sc-expand (list 'lambda '(%sc-val) setfns)))
                       method-values)))
          (list var localstate)))

       (expand-getfns
         (lambda (var getfns)
           ((rec loop
              (lambda (var gets exp-form)
                (if gets
                    (loop
                      var
                      (cdr gets)
                      (list (car gets) exp-form))
                    exp-form)))
            var getfns var)))

       (concat
         (lambda (str sym)
           (string->symbol (string-append str (symbol->string sym)))))

       (generate-get
         (lambda (getlist)
           (mapcar
             (lambda (a)
               (list 'cons (list 'quote (concat "GET-" a))
                     (%sc-expand (list 'lambda '() a))))
             getlist)))

       (generate-set
         (lambda (setlist)
           (mapcar
             (lambda (a)
               (list 'cons (list 'quote (concat "SET-" a))
                           (%sc-expand
                               (list 'lambda '(%sc-val)
                                     (list 'set! a '%sc-val)))))
             setlist)))

     )

        (chk-class-def (cddr e))
        (set! method-values
              (chk-option
                  (mapcar (lambda (a) (if (atom? a) (cons a allvars) a))
                          options)))
        (list 'define
              name
              (list '%sc-make-class
                    (list 'quote name)
                    (if classvars
                        (list 'quote (chk-cvs classvars))
                        '())
                    (if instvars
                        (list 'quote (chk-ivs instvars))
                        '())
                    (list 'quote mixins)
                    (if method-values
                        (cons 'list method-values)
                        '())
                    ))))))
