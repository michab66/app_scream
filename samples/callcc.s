;; $Id: callcc.s 10 2008-09-14 22:56:15Z binzm $
;;
;; Scream / samples
;;
;; call-with-current-continuation test case.
;;
;;
;; Released under Gnu Public License
;; Copyright (c) 2008 Michael G. Binz

(define (search wanted? lst)
	(call-with-current-continuation
		(lambda (return)
			(for-each
				(lambda (element)
					(if (wanted? element)
						(return element)))
				lst)
			#f)))


;;; This one must return 4.
(call/cc
  (lambda (k)
    (* 5 (k 4))))
