
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

(define (backward-one-list start end)
  (backward-sexp:top start end 0))

(define backward-sexp:top
  (lambda (start end depth)
    (letrec
      ((backward-sexp:top
         (lambda (start end depth)
           (and (mark> start end)
                (search-backward start end depth))))

       (search-backward
         (lambda (start end depth)
           (let ((mark (find-previous-char-in-set start end sexp-delims)))
             (and mark
                  (cond
                   ((char=? (mark-left-char mark)   ;;; (
                            #\) )
                    (list-backward-close (mark-1+ mark #!false) end depth))
                   (else
                    (if (and (<>? depth 1)
                             (terminate? mark))
                        #!false
                        (list-backward-open (mark-1+ mark #!false)
                                            end depth))))))))
      (terminate?
        (lambda (mark)
          (and (= 1 (mark-position mark))
               (let ((m (line-start mark -1 #!false)))
                 (and m
                      (line-blank? m))))))

      (list-backward-close
        (lambda (start end depth)
          (if (= depth -1)
              start
              (backward-sexp:top start end (1+ depth)))))

      (list-backward-open
        (lambda (start end depth)
          (and (> depth 0)
               (if (= depth 1)
                   start
                   (backward-sexp:top start end (-1+ depth)))))))
    (backward-sexp:top start end depth))))

(define with-reverse-attributes
  (let ((reverse-attr
          (if (= pcs-machine-type  1) ;;; recode for unknown machine type
              31            ;;; TI
              112))         ;;; IBM
        (display-matching-paren
          (lambda (old)
            (let ((x (%reify-port buffer-screen screen:cursor-x))
                  (y (%reify-port buffer-screen screen:cursor-y)))
              (princ #\( buffer-screen)           ;;;;)
              (delay-input 50 buffer-screen)
              (%reify-port! buffer-screen 7 old)
              (%reify-port! buffer-screen screen:cursor-x x)
              (%reify-port! buffer-screen screen:cursor-y y)
              (princ #\( buffer-screen)))))            ;;;;;)
  (lambda ()
    (let ((old (%reify-port buffer-screen 7)))
      (update-display! (current-window))
      (%reify-port! buffer-screen 7 reverse-attr)
      (display-matching-paren old)))))


(define delay-input
  (let ((delay-time
         (if (= pcs-machine-type 1)      ;;; recode for unknown machine type
             500            ;;; TI
             1200)))        ;;; IBM

  (lambda (n screen)
    ((rec loop
      (lambda (n)
        (if (char-ready? screen)
            #!true
            (if (zero? n)
                #!false
                (loop (-1+ n)))))) delay-time))))
