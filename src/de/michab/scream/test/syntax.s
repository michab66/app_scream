; $Id: syntax.s 199 2009-08-03 21:34:01Z Michael $
;
; Scream / Regression test
;
; Released under Gnu Public License
; Copyright (c) 1998-2000 Michael G. Binz

; Tests the Scream language syntax implementations.

;;
;; Test setup
;;
(define sourcefile "test/syntax.s")

; define
(%positive-test sourcefile 1
  (define i 1)
  ())
(%positive-test sourcefile 2
  (equal? i 1)
  #t)
;; TODO The following tests are not working.  i will
;; always have the value 1.
(%positive-test sourcefile 3
  (define i "bluh")
  ())
(%positive-test sourcefile 4
  (equal? i "bluh")
  #t)
(%positive-test sourcefile 5
  (define i #(1 2 3))
  ())
(%positive-test sourcefile 6
  (equal? i #(1 2 3))
  #t)

(%positive-test sourcefile 7
  (define i ())
  ())
(%positive-test sourcefile 8
  (equal? i ())
  #t)
  
(%positive-test sourcefile 7
  (define i (lambda (x) (+ x 2)))
  ())
(%positive-test sourcefile 8
  (equal? (i 2) 4)
  #t)
  
(%positive-test sourcefile 7
  (define (i x) (+ x 2))
  ())
(%positive-test sourcefile 8
  (equal? (i 2) 4)
  #t)

(%negative-test sourcefile 18
  (define () 1)
  4 ) ; Syntax-error

(define (i . rest) rest)

; if
(if 1 2 3) ; -> 2
(if () 2 3) ; -> 2
(if #f 2 3) ; -> 3)
(if 1 2) ; -> 2
(if #f 2) ; -> NIL

; do

; let

;
