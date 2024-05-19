;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 1998-2023 Michael G. Binz
;

;;
;; Scream definitions.
;;



;;
;; r7rs definitions.
;;

;;
;; (string? obj) procedure; r7rs 46
;;
(define string?
  scream:string?)

;;
;; (make-string k) procedure; r7rs 46
;; (make-string k char) procedure; r7rs 46
;;
(define make-string

(scream:delay-op (delay ; -->

  (case-lambda

    ((k)
      (make-string k #\space))

    ((k char)
      (begin
        (scream:assert:integer 'make-string k 1)
        (scream:assert:char 'make-string char 2)
        (make-object ("de.michab.scream.fcos.SchemeString:int,char" k char))
      )
    )
  ) ; case-lambda

  )) ; <--
)


;;
;; (string char ...) library procedure; r7rs 46
;;
(define (string char . optional-chars)
  (scream:assert:char 'string char 1)
  (do
    ; Init
    ((result (make-string (+ 1 (length optional-chars)) char))
     (insert-position 1 (+ 1 insert-position))
     (insert-list optional-chars (cdr insert-list)))

    ; Test
    ((null? insert-list) result)

    ; Body
    ((object result) ("setCharAt:int,char" 
                       insert-position
                       (scream:assert:char 'string (car insert-list) (+ 1 insert-position))))
  )
)



;;
;; (string-length string) procedure; r7rs 46
;;
(define (string-length string)
  (scream:assert:string 'string-length string) 
  ((object string) ("length"))
)



;;
;; (string-ref string k) procedure; r7rs 46
;;
(define (string-ref string k)
  (scream:assert:string 'string-ref string 1) 
  (scream:assert:integer 'string-ref k 2) 
  ((object string) ("getCharAt:int" k))
)

;;
;; (string-set! string k char) procedure; r7rs 46
;;
(define (string-set! string k char)
  (scream:assert:string 'string-set! string 1)
  (scream:assert:integer 'string-set! k 2)
  (scream:assert:char 'string-set! char 3)
  ((object string) ("setCharAt:int,char" k char))
)

;;
;; (substring string start end) library procedure; r7rs 47
;;
(define (substring string start end)
  (scream:assert:string 'substring string 1)
  (scream:assert:integer 'substring start 2)
  (scream:assert:integer 'substring end 3)
  ((object string) ("substring:int,int" start end))
)

;;
;; (string-append string ...) library procedure; r7rs 47
;;

;; TODO use (scream:string-append a b)
(define (string-append string . optional-string-list)
  (scream:assert:string 'string-append string 1)
  (do
    ; Init
    ((result ((make-object "de.michab.scream.fcos.SchemeString") ("makeEscaped:java.lang.String" string)))
      (append-list optional-string-list (cdr append-list)))

    ; Test
    ((null? append-list) result)

    ; Body
    (if (string? (car append-list))
      (set! result ((object result) ("append:de.michab.scream.fcos.SchemeString" (car append-list))))
      (error
        "TYPE_ERROR"
        scream:type:string
        (scream:typename (car append-list))
        ; Complex parameter position calculation.  The difference between
        ; the length of the original input list and the rest of that list
        ; that is left for processing plus an offset of 2 because of the
        ; static parameter.
        (+ 2 (- (length optional-string-list) (length append-list)))
      )
    )
  )
)

;;
;; (string->list string) library procedure; r7rs 47
;;
(define (string->list string)
  (scream:assert:string 'string->list string)
  ((object string) ("toCons"))
)

;;
;; (list->string list) library procedure; r7rs 47
;;
(define (list->string character-list)
  (if (list? character-list)
    (do
      ; Init
      ((result (make-string (length character-list)))
       (remaining-characters character-list (cdr remaining-characters))
       (idx 0 (+ idx 1)))

      ; Test
      ((null? remaining-characters) result)

      ; Body
      (if (char? (car remaining-characters))
        (string-set! result idx (car remaining-characters))
        (error "TYPE_ERROR"
               scream:type:char
               (scream:typename (car remaining-characters))
               (+ 1 idx))))
    (error "TYPE_ERROR"
           scream:type:cons
           (scream:typename character-list)
           1)))

;;
;; (string-copy string) library procedure; r7rs 47
;;
(define (string-copy string)
  (scream:assert:string 'string-copy string)
  ((object string) ("copy"))
)

;;
;; (string-fill! string char) library procedure; r7rs 48
;;
(define (string-fill! string char)
  (scream:assert:string 'string-fill! string 1)
  (scream:assert:char 'string-fill! char 2)
  ((object string) ("fill:char" char))
)

;;
;; Comparison procedures.
;;
(define (string=? s1 s2)
  (scream:assert:string 'string=? s1 1)
  (scream:assert:string 'string=? s2 2)
  (= 0 ((object s1) ("compareTo:de.michab.scream.fcos.SchemeString" s2)))
)

(define (string-ci=? s1 s2)
  (scream:assert:string 'string-ci=? s1 1)
  (scream:assert:string 'string-ci=? s2 2)
  (= 0 ((object s1) ("compareToIgnoreCase:de.michab.scream.fcos.SchemeString" s2)))
)

(define (string<? s1 s2)
  (scream:assert:string 'string<? s1 1)
  (scream:assert:string 'string<? s2 2)
  (> 0 ((object s1) ("compareTo:de.michab.scream.fcos.SchemeString" s2)))
)

; TODO NO TEST
(define (string-ci<? s1 s2)
  (scream:assert:string 'string-ci< s1 1)
  (scream:assert:string 'string-ci< s2 2)
  (> 0 ((object s1) ("compareToIgnoreCase:de.michab.scream.fcos.SchemeString" s2)))
)

(define (string>? s1 s2)
  (scream:assert:string 'string>? s1 1)
  (scream:assert:string 'string>? s2 2)
  (< 0 ((object s1) ("compareTo:de.michab.scream.fcos.SchemeString" s2)))
)

(define (string-ci>? s1 s2)
  (scream:assert:string 'string-ci>? s1 1)
  (scream:assert:string 'string-ci>? s2 2)
  (< 0 ((object s1) ("compareToIgnoreCase:de.michab.scream.fcos.SchemeString" s2)))
)

(define (string<=? s1 s2)
  (scream:assert:string 'string<=? s1 1)
  (scream:assert:string 'string<=? s2 2)
  (>= 0 ((object s1) ("compareTo:de.michab.scream.fcos.SchemeString" s2)))
)

(define (string-ci<=? s1 s2)
  (scream:assert:string 'string-ci<=? s1 1)
  (scream:assert:string 'string-ci<=? s2 2)
  (>= 0 ((object s1) ("compareToIgnoreCase:de.michab.scream.fcos.SchemeString" s2)))
)

(define (string>=? s1 s2)
  (scream:assert:string 'string>=? s1 1)
  (scream:assert:string 'string>=? s2 2)
  (<= 0 ((object s1) ("compareTo:de.michab.scream.fcos.SchemeString" s2)))
)

(define (string-ci>=? s1 s2)
  (scream:assert:string 'string-ci>=? s1 1)
  (scream:assert:string 'string-ci>=? s2 2)
  (<= 0 ((object s1) ("compareToIgnoreCase:de.michab.scream.fcos.SchemeString" s2)))
)
