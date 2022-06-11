; $Id: quasiquote.s 187 2009-06-21 14:32:28Z Michael $
;
; Scream / Regression test
;
; Released under Gnu Public License
; Copyright (c) 1998-2000 Michael G. Binz

;;
;; Test setup
;;
(define sourcefile "test/quasiquote.s")

;;
;; Tests 1 to n are the quasiquote examples from R5RS 4.2.6 p13
;;

;;
;; Test 1
;;
(%positive-test sourcefile 1
  `(list ,(+ 1 2) 4)
  (list 3 4))

;;
;; Test 2
;;
(%positive-test sourcefile 2
  (let ((name 'a)) `(list ,name ',name))
  (list a (quote a)))

;;
;; Test 3
;;
(%positive-test sourcefile 3
  `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)
  (a 3 4 5 6 b))

;;
;; Test 4
;;
(%positive-test sourcefile 4
  `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))
  ((foo 7) . cons))

;;
;; Test 5
;;
;; Note that Scream currently handles floating point numbers in sqrt differnt
;; from other implementations.  That is the reason for the floats in the
;; expected result.
;;
(%positive-test sourcefile 5
  `#(10 5 ,(sqrt 4.0) ,@(map sqrt '(16.0 9.0)) 8)
  #(10 5 2.0 4.0 3.0 8))

;;
;; Test 6
;;
(%positive-test sourcefile 6
  `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)
  (a `(b ,(+ 1 2) ,(foo 4 d) e) f))

;;
;; Test 7
;;
(%positive-test sourcefile 7
  (let ((name1 'x)
        (name2 'y))
    `(a `(b ,,name1 ,',name2 d) e))
  (a `(b ,x ,'y d) e))
