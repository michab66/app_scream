; $Id: tstring.s 187 2009-06-21 14:32:28Z Michael $
;
; Scream / Regression test
;
; Released under Gnu Public License
; Copyright (c) 1998-2000 Michael G. Binz

;;
;; Test setup
;;
(define sourcefile "test/tstring.s")

;;
;; (string? obj) procedure; r5rs 30
;;
;;(define string?
;;  (typePredicateGenerator "de.michab.scream.SchemeString" #t))



;;
;; (make-string k) procedure; r5rs 30
;; (make-string k char) procedure; r5rs 30
;;
(%positive-test sourcefile 1
  (make-string 5)
  "     ")
(%positive-test sourcefile 2
  (make-string 5 #\x)
  "xxxxx")


;;
;; (string char ...) library procedure; r5rs 30
;;
(%positive-test sourcefile 3
  (string #\D #\o #\n #\a #\l #\d)
  "Donald")



;;
;; (string-length string) procedure; r5rs 30
;;
(%positive-test sourcefile 4
  (string-length "Donald")
  6)

;;
;; (string-ref string k) procedure; r5rs 30
;;
(%positive-test sourcefile 5
  (string-ref "Donald" 4)
  #\l)

;;
;; (string-set! string k char) procedure; r5rs 30
;;

;;
;; (substring string start end) library procedure; r5rs 30
;;
(%positive-test sourcefile 6
  (substring "Lumumba" 1 5)
  "umum")

;;
;; (string-append string ...) library procedure; r5rs 30
;;
(%positive-test sourcefile 7
  (string-append "Donald" )
  "Donald")
(%positive-test sourcefile 8
  (string-append "Donald" " " "Duck")
  "Donald Duck")

;;
;; (string->list string) library procedure; r5rs 30
;;
(%positive-test sourcefile 9
  (string->list "Donald Duck")
  (#\D #\o #\n #\a #\l #\d #\space #\D #\u #\c #\k))

;;
;; (list->string list) library procedure; r5rs 30
;;
(%positive-test sourcefile 10
  (list->string '(#\D #\o #\n #\a #\l #\d #\space #\D #\u #\c #\k))
  "Donald Duck")

;;
;; (string-copy string) library procedure; r5rs 30
;;
(%positive-test sourcefile 11
  (string-copy "Schlawinia Kümmelbein")
  "Schlawinia Kümmelbein")

;;
;; (string-fill! string char) library procedure; r5rs 31
;;

;;
;; Comparison procedures.
;;
(%positive-test sourcefile 12
  (string=? "A" "A")
  #t)
(%positive-test sourcefile 13
  (string=? "a" "A")
  #f)

(%positive-test sourcefile 14
  (string-ci=? "a" "A")
  #t)
(%positive-test sourcefile 15
  (string-ci=? "A" "b")
  #f)

(%positive-test sourcefile 16
  (string<? "a" "c")
  #t)
(%positive-test sourcefile 17
  (string<? "b" "b")
  #f)
(%positive-test sourcefile 18
  (string<? "c" "a")
  #f)

(%positive-test sourcefile 19
  (string-ci<? "A" "c")
  #t)
(%positive-test sourcefile 20
  (string-ci<? "B" "b")
  #f)
(%positive-test sourcefile 21
  (string-ci<? "C" "a")
  #f)

(%positive-test sourcefile 22
  (string>? "a" "c")
  #f)
(%positive-test sourcefile 23
  (string>? "b" "b")
  #f)
(%positive-test sourcefile 24
  (string>? "c" "a")
  #t)

(%positive-test sourcefile 25
  (string-ci>? "A" "c")
  #f)
(%positive-test sourcefile 26
  (string-ci>? "B" "b")
  #f)
(%positive-test sourcefile 27
  (string-ci>? "C" "a")
  #t)

(%positive-test sourcefile 28
  (string<=? "a" "c")
  #t)
(%positive-test sourcefile 29
  (string<=? "b" "b")
  #t)
(%positive-test sourcefile 30
  (string<=? "c" "a")
  #f)

(%positive-test sourcefile 31
  (string-ci<=? "A" "c")
  #t)
(%positive-test sourcefile 32
  (string-ci<=? "B" "b")
  #t)
(%positive-test sourcefile 33
  (string-ci<=? "C" "a")
  #f)

(%positive-test sourcefile 34
  (string>=? "a" "c")
  #f)
(%positive-test sourcefile 35
  (string>=? "b" "b")
  #t)
(%positive-test sourcefile 36
  (string>=? "c" "a")
  #t)

(%positive-test sourcefile 37
  (string-ci>=? "a" "c")
  #f)
(%positive-test sourcefile 38
  (string-ci>=? "b" "b")
  #t)
(%positive-test sourcefile 39
  (string-ci>=? "c" "a")
  #t)
