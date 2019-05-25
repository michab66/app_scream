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
;;;     Modified by Texas Instruments Inc 10/21/85
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define source-dir "")
(define fastload-dir "")
(define dist-dir "")

(define dev)
(begin
;;(writeln "Optimize define integrables ? ")
;;(if (eq? (read) 'y)
      (begin
        (set! dev #!true)
        (load (string-append source-dir "de.scm")))
;;    (begin
;;      (set! dev #!false))
   )

(define Version
  ((rec loop
    (lambda ()
;;;   (writeln "Enter Version Number for Edwin (string) : ")
      (let ((version "3.03"))
           (if (string? version)
               version
               (begin
                 (writeln "Please use a string")
                 (loop))))))))

(macro make-version
  (lambda (exp)
    `(define-integrable edwin-version ,version)))
(make-version)

(define load-file
  (let ((n 1))
    (lambda (file)
     (if (< n stop-files)
      (begin
      (if (> n skip-files)
          (let ((file1 (string-append source-dir file ".scm"))
                (file2 (string-append (if dev dist-dir fastload-dir)
                                      file ".so")))
            (writeln "Compiling File : " file1)
            (gc)
            (compile-file file1 file2)
            (writeln "File " file1 " compiled to " file2)
            (if (not dev)
                (dos-call "" (string-append "make_fsl "
                                            file ".so"
                                            " "
                                            file
                                            ".f"))))
          (let ((fsl (string-append fastload-dir file ".f")))
            (writeln "Fast Loading " fsl)
            (fast-load fsl)))
      (set! n (+ n 1)))))))

(define skip-files 0)
(define stop-files 1000)
(define ld
  (lambda (no)
    (set! skip-files no)))
(define ls
  (lambda (no)
    (set! stop-files no)))



