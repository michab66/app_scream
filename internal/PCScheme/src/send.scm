

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                 ;;;
;;;                     S c o o p s                                 ;;;
;;;                                                                 ;;;
;;;      (c) Copyright 1985 Texas Instruments Incorporated          ;;;
;;;                  All Rights Reserved                            ;;;
;;;                                                                 ;;;
;;;               File updated : 8/29/85                            ;;;
;;;                                                                 ;;;
;;;                   File : send.scm                               ;;;
;;;                                                                 ;;;
;;;                 Amitabh Srivastava                              ;;;
;;;                                                                 ;;;
;;;    This file contains the send macro. This utilizes an          ;;;
;;;    internal hack for speed.                                     ;;;
;;;                                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;

(macro send
  (let ((names (vector 'scoop-send-handler-0
                       'scoop-send-handler-1
                       'scoop-send-handler-2
                       'scoop-send-handler-3
                       'scoop-send-handler-4
                       'scoop-send-handler-5
                       'scoop-send-handler-6
                       'scoop-send-handler-7
                       'scoop-send-handler-8
                       'scoop-send-handler-9
                       'scoop-send-handler-10)))

    (lambda (e)
      (let ((args (cdddr e)))
        (let ((fn (vector-ref names (length args)))
              (msg (caddr e))
              (env (cadr e)))
          (list 'let 
                (list (list '%sc-env env))
                (append (cons fn args)
                        (list (list 'access msg '%sc-env) '%sc-env))))))))




;;; send-if-handles

(macro send-if-handles
  (lambda (e)
    (let ((obj (cadr e))
          (msg (caddr e))
          (args (cdddr e)))
      (list 'let 
            (list (list '%sc-env obj))
            (list 'if
                  (list 'assq 
                        (list 'quote msg)
                        '(%sc-method-structure (access %sc-class %sc-env)))
                  (cons 'send (cons '%sc-env (cddr e)))
                  '())))))

