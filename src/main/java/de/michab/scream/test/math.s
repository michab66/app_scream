; $Id: math.s 187 2009-06-21 14:32:28Z Michael $
;
; Scream / Regression test
;
; Released under Gnu Public License
; Copyright (c) 1998-2000 Michael G. Binz

;;
;; Test setup
;;
(define sourcefile "test/math.s")

;;
;; R5RS 4.2.6 p21
;;

;;
;; Test =
;;
(%positive-test sourcefile 1
  (= 1 1.0 (- 3 2) (* .5 2))
  #t)
(%positive-test sourcefile 2
  (= 1 1.0 (- 3 2) (* .5 3))
  #f)

;;
;; Test <=>
;;
(%positive-test sourcefile 3
  (<= 1 1.0 1.5 2.0 2)
  #t)
(%positive-test sourcefile 4
  (<= 1 1.5 1)
  #f)
(%positive-test sourcefile 5
  (< 1 1.5 2)
  #t)
(%positive-test sourcefile 6
  (< 1 1.0 1.5 2.0 2)
  #f)

(%positive-test sourcefile 7
  (> 1 0.0 -1)
  #t)
(%positive-test sourcefile 8
  (> 1 0.0 1)
  #f)
(%positive-test sourcefile 9
  (>= 1 1.0 1 0.0 0 0.0 -1 -1.0 -1)
  #t)
(%positive-test sourcefile 10
  (>= 1 1.0 1.5 2.0 2)
  #f)

;;
;;
;;
(%positive-test sourcefile 11
  (zero? 0)
  #t)
(%positive-test sourcefile 12
  (zero? 0.0)
  #t)
(%positive-test sourcefile 13
  (zero? 1)
  #f)
(%positive-test sourcefile 14
  (zero? -1.0)
  #f)

;;
;;
;;
(%positive-test sourcefile 15
  (positive? 0)
  #f)
(%positive-test sourcefile 16
  (positive? -1)
  #f)
(%positive-test sourcefile 17
  (positive? 1)
  #t)
(%positive-test sourcefile 18
  (negative? 0)
  #f)
(%positive-test sourcefile 19
  (negative? -1)
  #t)
(%positive-test sourcefile 20
  (negative? 1)
  #f)

;;
;;
;;
(%positive-test sourcefile 21
  (odd? 1)
  #t)
(%positive-test sourcefile 22
  (odd? 2)
  #f)
(%positive-test sourcefile 23
  (even? 1)
  #f)
(%positive-test sourcefile 24
  (even? 2)
  #t)
(%positive-test sourcefile 25
  (even? 0)
  #t)

;;;
(%positive-test sourcefile 26
  (max 3 4 5)
  5)
(%positive-test sourcefile 27
  (min 3 4 5)
  3)

;; +*
(%positive-test sourcefile 28
  (+ 3 4)
  7)
(%positive-test sourcefile 29
  (+ 3)
  3)
(%positive-test sourcefile 30
  (+)
  0)

(%positive-test sourcefile 31
  (* 3 4)
  12)
(%positive-test sourcefile 32
  (* 4)
  4)
(%positive-test sourcefile 33
  (*)
  1)

;; -/
(%positive-test sourcefile 34
  (- 3 4)
  -1)
(%positive-test sourcefile 35
  (- 3 4 5)
  -6)
(%positive-test sourcefile 36
  (- 3)
  -3)
(%positive-test sourcefile 37
  (/ 3.0 4 5) ; TODO
  0.15)
(%positive-test sourcefile 38
  (/ 3.0)
  0.3333333333333333)

;;
(%positive-test sourcefile 39
  (abs -7)
  7)

;;
(%positive-test sourcefile 40
  (modulo 13 4)
  1)
(%positive-test sourcefile 41
  (remainder 13 4)
  1)
(%positive-test sourcefile 42
  (modulo -13 4)
  3)
(%positive-test sourcefile 43
  (remainder -13 4)
  -1)
(%positive-test sourcefile 44
  (modulo 13 -4)
  -3)
(%positive-test sourcefile 45
  (remainder 13 -4)
  1)
(%positive-test sourcefile 46
  (modulo -13 -4)
  -1)
(%positive-test sourcefile 47
  (remainder -13 -4)
  -1)

;;
(%positive-test sourcefile 48
  (gcd 32 -36)
  4)
(%positive-test sourcefile 49
  (gcd)
  0)
(%positive-test sourcefile 50
  (lcm 32 -36)
  288)
(%positive-test sourcefile 51
  (lcm)
  1)

;;
(%positive-test sourcefile 52
  (floor -4.3)
  -5.0)
(%positive-test sourcefile 53
  (ceiling -4.3)
  -4.0)
(%positive-test sourcefile 54
  (truncate -4.3)
  -4.0)
(%positive-test sourcefile 55
  (round -4.3)
  -4.0)
(%positive-test sourcefile 56
  (floor 3.5)
  3.0)
(%positive-test sourcefile 57
  (ceiling 3.5)
  4.0)
(%positive-test sourcefile 58
  (truncate 3.5)
  3.0)
(%positive-test sourcefile 59
  (round 3.5)
  4.0)
