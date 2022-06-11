; $Id: tcontinuation.s 195 2009-08-03 20:28:12Z Michael $
;
; Scream / Regression test
;
; Released under Gnu Public License
; Copyright (c) 1998-2009 Michael G. Binz

;; $Rev: 195 $

;;
;; Test setup
;;
(define sourcefile "test/tcontinuation.s")

;;
;; (call-with-current-continuation k) procedure; r5rs 33
;;
(%positive-test sourcefile 1
  (call-with-current-continuation 
    (lambda (k) (* 5 (k 4))))
  4)

