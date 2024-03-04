;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 1998-2023 Michael G. Binz
;

;;
;; Scream definitions.
;;

;; String type name.
(define scream:type-string
  ((make-object "de.michab.scream.fcos.SchemeString") TYPE_NAME))



;;
;; r7rs definitions.
;;

;;
;; (string? obj) procedure; r5rs 30
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
      (cond
        ((not (integer? k))
          (error "TYPE_ERROR" scream:type-integer to))
        ((not (char? char))
          (error "TYPE_ERROR" scream:type-character from))
        (else
          (make-object ("de.michab.scream.fcos.SchemeString:int,char" k char)))))
  ) ; case-lambda

  )) ; <--
)


;;
;; (string char ...) library procedure; r5rs 30
;;
(define (string char . optional-chars)
  (if (char? char)
    (do
      ; Init
      ((result (make-string (+ 1 (length optional-chars)) char))
       (insert-position 1 (+ 1 insert-position))
       (insert-list optional-chars (cdr insert-list)))

      ; Test
      ((null? insert-list) result)

      ; Body
      (if (char? (car insert-list))
        ((object result) (setCharAt insert-position (car insert-list)))
        (error "TYPE_ERROR"
               %type-char
               (scream:typename (car insert-list))
               (+ 1 insert-position))))

    ;; Error handler for first argument slot.
    (error "TYPE_ERROR" %type-char (scream:typename char) 1)))



;;
;; (string-length string) procedure; r5rs 30
;;
(define (string-length string)
  (if (string? string)
    ((object string) (length))
    (error "TYPE_ERROR" %type-string (scream:typename string))))



;;
;; (string-ref string k) procedure; r5rs 30
;;
(define (string-ref string k)
  (if (string? string)
    ((object string) (getCharAt k))
    (error "TYPE_ERROR" %type-string (scream:typename string) 1)))


;;
;; (string-set! string k char) procedure; r5rs 30
;;
(define (string-set! string k char)
  (if (not (string? string))
    (error "TYPE_ERROR" %type-string (scream:typename string) 1))
  (if (not (integer? k))
    (error "TYPE_ERROR" %type-integer (scream:typename k) 2))
  (if (not (char? char))
    (error "TYPE_ERROR" %type-char (scream:typename char) 3))

  ((object string) (setCharAt k char)))



;;
;; (substring string start end) library procedure; r5rs 30
;;
(define (substring string start end)
  (if (not (string? string))
    (error "TYPE_ERROR" %type-string (scream:typename string) 1))
  (if (not (integer? start))
    (error "TYPE_ERROR" %type-integer (scream:typename start) 2))
  (if (not (integer? end))
    (error "TYPE_ERROR" %type-integer (scream:typename end) 3))
  ((object string) (substring start end)))



;;
;; (string-append string ...) library procedure; r5rs 30
;;
(define (string-append string . optional-string-list)
  (if (string? string)
    (do
      ; Init
      ((result ((make-object "de.michab.scream.fcos.SchemeString") (makeEscaped string)))
       (append-list optional-string-list (cdr append-list)))

      ; Test
      ((null? append-list) result)

      ; Body
      (if (string? (car append-list))
        (set! result ((object result) (append (car append-list))))
        (error
          "TYPE_ERROR"
          %type-string
          (scream:typename (car append-list))
          ; Complex parameter position calculation.  The difference between
          ; the length of the original input list and the rest of that list
          ; that is left for processing plus an offset of 2 because of the
          ; static parameter.
          (+ 2 (- (length optional-string-list) (length append-list))))))
    (error "TYPE_ERROR" %type-string (scream:typename string) 1)))



;;
;; (string->list string) library procedure; r5rs 30
;;
(define (string->list string)
  (if (string? string)
    ((object string) (toCons))
    (error "TYPE_ERROR" %type-string (scream:typename string))))



;;
;; (list->string list) library procedure; r5rs 30
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
               %type-char
               (scream:typename (car remaining-characters))
               (+ 1 idx))))
    (error "TYPE_ERROR"
           %type-cons
           (scream:typename character-list)
           1)))



;;
;; (string-copy string) library procedure; r5rs 30
;;
(define (string-copy string)
  (if (string? string)
    ((object string) (copy))
    (error "TYPE_ERROR" %type-string (scream:typename string))))



;;
;; (string-fill! string char) library procedure; r5rs 31
;;
(define (string-fill! string char)
  (if (not (string? string))
    (error "TYPE_ERROR" %type-string (scream:typename string) 1))
  (if (not (char? char))
    (error "TYPE_ERROR" %type-char (scream:typename char) 2))

  ((object string) (fill char)))



;;
;; Comparison procedures.
;;
(define (string=? s1 s2)
  (if (not (string? s1))
    (error "TYPE_ERROR" %type-string (scream:typename s1) 1))
  (if (not (string? s2))
    (error "TYPE_ERROR" %type-string (scream:typename s1) 2))
  (= 0 ((object s1) (compareTo s2))))

; Note that this operation was fixed but the others
; remain with errors, 
(define (string-ci=? s1 s2)
  (if (not (string? s1))
    (error "TYPE_ERROR" scream:type-string s1 1))
  (if (not (string? s2))
    (error "TYPE_ERROR" scream:type-string s2 2))
  (= 0 ((object s1) (compareToIgnoreCase s2))))

(define (string<? s1 s2)
  (if (not (string? s1))
    (error "TYPE_ERROR" %type-string (scream:typename s1) 1))
  (if (not (string? s2))
    (error "TYPE_ERROR" %type-string (scream:typename s1) 2))
  (> 0 ((object s1) (compareTo s2))))

(define (string-ci<? s1 s2)
  (if (not (string? s1))
    (error "TYPE_ERROR" %type-string (scream:typename s1) 1))
  (if (not (string? s2))
    (error "TYPE_ERROR" %type-string (scream:typename s1) 2))
  (> 0 ((object s1) (compareToIgnoreCase s2))))

(define (string>? s1 s2)
  (if (not (string? s1))
    (error "TYPE_ERROR" %type-string (scream:typename s1) 1))
  (if (not (string? s2))
    (error "TYPE_ERROR" %type-string (scream:typename s1) 2))
  (< 0 ((object s1) (compareTo s2))))

(define (string-ci>? s1 s2)
  (if (not (string? s1))
    (error "TYPE_ERROR" %type-string (scream:typename s1) 1))
  (if (not (string? s2))
    (error "TYPE_ERROR" %type-string (scream:typename s1) 2))
  (< 0 ((object s1) (compareToIgnoreCase s2))))

(define (string<=? s1 s2)
  (if (not (string? s1))
    (error "TYPE_ERROR" %type-string (scream:typename s1) 1))
  (if (not (string? s2))
    (error "TYPE_ERROR" %type-string (scream:typename s1) 2))
  (>= 0 ((object s1) (compareTo s2))))

(define (string-ci<=? s1 s2)
  (if (not (string? s1))
    (error "TYPE_ERROR" %type-string (scream:typename s1) 1))
  (if (not (string? s2))
    (error "TYPE_ERROR" %type-string (scream:typename s1) 2))
  (>= 0 ((object s1) (compareToIgnoreCase s2))))

(define (string>=? s1 s2)
  (if (not (string? s1))
    (error "TYPE_ERROR" %type-string (scream:typename s1) 1))
  (if (not (string? s2))
    (error "TYPE_ERROR" %type-string (scream:typename s1) 2))
  (<= 0 ((object s1) (compareTo s2))))

(define (string-ci>=? s1 s2)
  (if (not (string? s1))
    (error "TYPE_ERROR" %type-string (scream:typename s1) 1))
  (if (not (string? s2))
    (error "TYPE_ERROR" %type-string (scream:typename s1) 2))
  (<= 0 ((object s1) (compareToIgnoreCase s2))))
