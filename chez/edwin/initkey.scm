;;;
;;;     Copyright (c) 1985 Massachusetts Institute of Technology
;;;
;;;     This material was developed by the Scheme project at the
;;;     Massachusetts Institute of Technology, Department of
;;;     Electrical Engineering and Computer Science.  Permission to
;;;     copy this software, to redistribute it, and to use it for any
;;;     purpose is granted, subject to the following restrictions and
;;;     understandings.
;;;
;;;     1. Any copy made of this software must include this copyright
;;;     notice in full.
;;;
;;;     2. Users of this software agree to make their best efforts (a)
;;;     to return to the MIT Scheme project any improvements or
;;;     extensions that they make, so that these may be included in
;;;     future releases; and (b) to inform MIT of noteworthy uses of
;;;     this software.
;;;
;;;     3.  All materials developed as a consequence of the use of
;;;     this software shall duly acknowledge such use, in accordance
;;;     with the usual standards of acknowledging credit in academic
;;;     research.
;;;
;;;     4. MIT has made no warrantee or representation that the
;;;     operation of this software will be error-free, and MIT is
;;;     under no obligation to provide any services, by way of
;;;     maintenance, update, or otherwise.
;;;
;;;     5.  In conjunction with products arising from the use of this
;;;     material, there shall be no use of the name of the
;;;     Massachusetts Institute of Technology nor of any adaptation
;;;     thereof in any advertising, promotional, or sales literature
;;;     without prior written consent from MIT in each case.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;     Modified by Texas Instruments Inc 8/15/85
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define define-initial-key
  (letrec
    ((%prefix
       (lambda (alists char)
         (let ((upcase (char-upcase char)))
           (%set-comtab-entry! alists upcase %command)
           (if (char-alphabetic? char)
               (%set-comtab-entry! alists (char-downcase char) %command)))))
     (%command '()))

    (lambda (char command)
      (cond ((char? char)
             (%set-comtab-key comtab (char-upcase char) command)
             (if (char-alphabetic?  char)
                 (%set-comtab-key comtab (char-downcase char) command)))
            ((and (pair? char) (null? (cdr char)))
             (%set-comtab-key comtab (char-upcase (car char)) command)
             (if (char-alphabetic?  (car char))
                 (%set-comtab-key comtab (char-downcase (car char)) command)))
            ((pair? char)
             (set! %command command)
             (comtab-lookup-prefix char %prefix))
          ((char-set? char)
           (mapc (lambda (char) (set-comtab-entry! char command))
                 (char-set-members char)))
          (else (error "Unknown character" char)))
      char)))

(define define-initial-prefix-key
  (letrec
    ((%prefix
        (lambda (alists char)
          (let ((upcase (char-upcase char)))
            (if (pair? %char)
                (%set-comtab-entry! alists upcase %command)
                (%set-comtab-key alists upcase %command))
            (%comtab-make-prefix-char! alists upcase (cons '() '()))
            (if (char-alphabetic? char)
                (%comtab-make-synonym-char! alists (char-downcase char)
                                            alists upcase)))))
     (%char '())
     (%command '()))

  (lambda (char command)
    (cond ((or (char? char) (pair? char))
           (set! %command command)
           (set! %char char)
           (comtab-lookup-prefix char %prefix))
          (else (error "Unknown character" char)))
    char)))

(define (define-initial-default-key command)
  (set! default-key command)
  (set-cdr! comtab (make-vector 256 command)))
                                ;;; change to 256 for internationalize
