

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                 ;;;
;;;                     S c o o p s                                 ;;;
;;;                                                                 ;;;
;;;      (c) Copyright 1985 Texas Instruments Incorporated          ;;;
;;;                  All Rights Reserved                            ;;;
;;;                                                                 ;;;
;;;               File updated : 5/16/85                            ;;;
;;;                                                                 ;;;
;;;                   File : debug.scm                              ;;;
;;;                                                                 ;;;
;;;                 Amitabh Srivastava                              ;;;
;;;                                                                 ;;;
;;;    This file contains routines to help in debugging             ;;;
;;;                                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; routines to help in debugging

(define print-class
  (lambda (class)
    (writeln "class name : " (%sc-name class))
    (writeln "class vars : " (%sc-cv class))
    (writeln "allcv      : " (%sc-allcvs class))
    (writeln "inst vars  : " (%sc-allivs class))
    (writeln "mixins     : " (%sc-mixins class))
    (writeln "template   : " (%sc-inst-template class))
    (writeln "method-env : " 
             (and (%sc-method-env class)
                  (environment-frame-bindings (%sc-method-env class))))
    (writeln "class-env  : "
             (and (%sc-class-env class)
                  (environment-frame-bindings (%sc-class-env class))))
    (writeln "method-str : " (%sc-method-structure class))
    (writeln "subclasses : " (%sc-subclasses class))
    (writeln "class-compiled : " (%sc-class-compiled class))
    (writeln "class-inherited : "(%sc-class-inherited class))
    (writeln "method values : " (%sc-method-values class))
 ))
