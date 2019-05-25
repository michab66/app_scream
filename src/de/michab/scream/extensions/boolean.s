; $Id: boolean.s 8 2008-09-14 14:23:20Z binzm $
;
; Scream / Boolean extensions
;
; Released under Gnu Public License
; Copyright (c) 1998-2000 Michael G. Binz



;;
;; (boolean? obj) library procedure; r5rs 25
;;
(define (boolean? obj)
  (or (eqv? obj #t) (eqv? obj #f)))



;;
;; (not obj) library procedure; r5rs 25
;;
(define (not obj)
  (eqv? obj #f))
