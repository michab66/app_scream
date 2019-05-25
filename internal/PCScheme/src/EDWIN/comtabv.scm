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


;;;; Command Tables

(define comtab '(()()))
(define default-key #!false)

(define (%set-comtab-key comtab char command)
  (vector-set! (cdr comtab) (char->integer char) command))

(define (%set-comtab-entry! alists char command)
  (let ((entry (assq char (cdr alists))))
    (if entry
        (set-cdr! entry command)
        (set-cdr! alists (cons (cons char command) (cdr alists))))))

(define (%comtab-make-prefix-char! alists char alists*)
  (let ((entry (assq char (car alists))))
    (if entry
        (set-cdr! entry alists*)
        (set-car! alists (cons (cons char alists*) (car alists))))))

(define (comtab-lookup-prefix char receiver)
  (define (loop char->alist chars)
    (let ((entry (assq (car chars) char->alist)))
      (if entry
          (if (null? (cddr chars))
              (receiver (cdr entry) (cadr chars))
              (loop (cadr entry) (cdr chars)))
          (error "Not a prefix character" (car chars)))))
  (cond ((char? char)
         (receiver comtab char))
        ((pair? char)
         (if (null? (cdr char))
             (receiver comtab (car char))
             (loop (car comtab) char)))
        (else
         (error "Unrecognizable character" char))))

(define comtab-entry
  (letrec
      ((ychar '())
       (receiver
         (lambda (alists char)
           (let ((entry (assq char (cdr alists))))
             (cond (entry (cdr entry))
                   (default-key default-key)
                   (t (editor-error (string-append "Unknown command: "
                                            (obj->string ychar)))))))))
    (lambda (xchar)
      (letrec
        ((lookup-vector
           (lambda (*char*)
             (let ((*int* (char->integer *char*)))
               (if (< *int* 256)      ;;; change to 256 for internationalize
                   (vector-ref (cdr comtab) *int*)
                   default-key)))))
        (cond ((char? xchar) (lookup-vector xchar))
              ((and (pair? xchar) (null? (cdr xchar)))
               (lookup-vector (car xchar)))
              (else (set! ychar xchar)
                    (comtab-lookup-prefix xchar receiver)))))))


(define (set-comtab-entry! char command)
  (comtab-lookup-prefix char
    (lambda (alists char)
      (%set-comtab-entry! alists char command))))

;;; These are not used becuase the initkey stuff is used to define keys

;;;(define (define-key char command-name)
;;;  (let ((command (name->command command-name)))
;;;    (cond ((char? char)
;;;           (%set-comtab-key comtab (char-upcase char) command)
;;;           (if (char-alphabetic?  char)
;;;               (%set-comtab-key comtab (char-downcase char) command)))
;;;          ((and (pair? char) (null? (cdr char)))
;;;           (%set-comtab-key comtab (char-upcase (car char)) command)
;;;           (if (char-alphabetic?  char)
;;;               (%set-comtab-key comtab (char-downcase (car char)) command)))
;;;          ((pair? char)
;;;        (comtab-lookup-prefix char
;;;          (lambda (alists char)
;;;            (let ((upcase (char-upcase char)))
;;;              (%set-comtab-entry! alists upcase command)
;;;              (if (char-alphabetic? char)
;;;                  (%set-comtab-entry! alists (char-downcase char)
;;;                                      command))))))
;;;       ((char-set? char)
;;;        (mapc (lambda (char) (set-comtab-entry! char command))
;;;              (char-set-members char)))
;;;       (else (error "Unknown character" char))))
;;;  char)
;;;
;;;(define (define-prefix-key char command-name)
;;;  (let ((command (name->command command-name)))
;;;    (cond ((or (char? char) (pair? char))
;;;        (comtab-lookup-prefix char
;;;          (lambda (alists char)
;;;            (let ((upcase (char-upcase char)))
;;;              (%set-comtab-key alists upcase command)
;;;              (%comtab-make-prefix-char! alists upcase (cons '() '()))
;;;              (if (char-alphabetic? char)
;;;                  (%comtab-make-synonym-char! alists (char-downcase char)
;;;                                              alists upcase))))))
;;;       (else (error "Unknown character" char))))
;;;  char)
;;;
;;;(define (define-default-key command-name)
;;;  (let ((command (name->command command-name)))
;;;    (set! default-key command)
;;;    (set-cdr! comtab (make-vector 128 command))))

