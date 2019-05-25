;; $Id: callcc2.s 23 2008-09-21 19:20:15Z binzm $
;;
;; Scream / samples
;;
;; call-with-current-continuation test case.
;;
;;
;; Released under Gnu Public License
;; Copyright (c) 2008 Michael G. Binz

(define return #f)

(+ 1 (call-with-current-continuation
        (lambda (cont)
            (set! return cont)
            1)))
