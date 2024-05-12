;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 2024 Michael G. Binz
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scream definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | (scream:files:validate-exists filename)
 |
 | Checks if a file exists.  If the file exists a file-object is returned.
 | Otherwise an IO_ERROR is thrown.
 |#
(define (scream:files:validate-exists filename)
  (let
    (
      (file (make-object ("java.io.File:java.lang.String" filename)))
    )

    (if (not (file ("exists")))
      (error "IO_ERROR" 'does-not-exist)
      file)))

#|
 | scream:files:current-dir
 |#
(define scream:files:current-dir
  "."
)

#|
 | (scream:files:list)
 | (scream:files:list filename)
 |
 | First form uses scream:files:current-dir.
 |#
(define scream:files:list

  (scream:delay-op (delay ; -->

  (case-lambda

    (()
      (scream:files:list scream:files:current-dir))

    ((dir)
      ((scream:files:validate-exists dir) ("list")))

  ) ; case-lambda

  )) ; <--
)

#|
 | (scream:files:create filename)
 |
 | Creates the named file.  Returns #t if the file was created, #f if it
 | already existed.
 |#
(define (scream:files:create filename)
  (let
    (
      (file (make-object ("java.io.File:java.lang.String" filename)))
    )
    
    (file ("createNewFile")))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; r7rs definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | (load filename)
 | (load filename environment-specifier)  file library procedure; r7rs p59
 |#
(define (load filename)
  (scream:evaluator 
    ("load:de.michab.scream.fcos.SchemeString,de.michab.scream.fcos.Environment" filename (interaction-environment)))
)

#|
 | (file-exists? filename)  file library procedure; r7rs p60
 |#
(define (file-exists? filename)
  (let
    (
      (file (make-object ("java.io.File:java.lang.String" filename)))
    )

    (file ("exists")))
)

#|
 | (delete-file filename)  file library procedure; r7rs p60
 |#
(define (delete-file filename)
  ((scream:files:validate-exists filename) ("delete")))

#|
 | (emergency-exit) process-context library procdure; r7rs p60
 |#
(define (emergency-exit)
  (scream:java:lang:system ("exit:int" 0)))

;;
;; Environment variable operations.
;;

#|
 | (get-environment-variable name)  process context library procedure; r7rs p60
 |#
(define (get-environment-variable name)
  (let*
    (
      (result
        (scream:java:lang:system ("getenv:java.lang.String" name)))
    )
    
    (if (null? result)
      #f
      result)
  )
)

;; TODO Candidate for string operations.
;; Get first index of char ins string.  Return index or #f.
(define (string-first char string)
  (define (_string-first idx char string)
    (cond
      ((= idx (string-length string))
        #f)
      ((eqv? char (string-ref string idx))
        idx) 
      (else
        (_string-first (+ idx 1) char string))))
  (_string-first 0 char string)
)

;; TODO Candidate for string operations.
;; Get last index of char ins string.  Return index or #f.
(define (string-last char string)
  (define (_string-last idx char string)
    (cond
      ((= idx -1)
        #f)
      ((eqv? char (string-ref string idx))
        idx)
      (else
        (_string-last (- idx 1) char string))))
  (_string-last (- (string-length string) 1) char string)
)

;; TODO Candidate for string operations.
;; MIT Scheme -- https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_7.html
(define (substring? pattern string)
;  (scream:display-ln 'substring? pattern string)
  (call/cc
(begin
;    (scream:display-ln 'call/cc pattern string)

    (lambda (exit)
;      (scream:display-ln 'lambda pattern string)
      (let*
        (
          (p-len
            (string-length pattern))
          (p-first
            (if (> p-len 0)
              (string-ref pattern 0)
              (exit 0)))
          (s-len
             (string-length string))
        )

;       (scream:display-ln p-len p-first s-len)

        (if (> p-len s-len)
          (exit #f))

        (define (_substring? position)
          (let*
            (
              (current-pos
                (string-first
                  p-first
                  (substring string position (string-length string))))
            )

            (if (not current-pos)
              (exit #f))
              
            (if (string-prefix? pattern (substring string current-pos (string-length string)))
              (exit current-pos)
              (_substring (+1 current-pos)))))

        (_substring? 0)
      ) ; let*
    ) ; lambda
  ) ; begin
  ) ; call/cc
)

;; TODO Candidate for string operations.
;; MIT Scheme -- https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_7.html
(define (string-prefix? prefix string)
  (let 
    (
      (prefix-len (string-length prefix))
      (string-len (string-length string))
    )
    (if (> prefix-len string-len)
      #f
      (equal?
        prefix
        (substring string 0 prefix-len)))
  )
)

; TODO Candidate for string operations.
(define (string-split pattern string)
  (define (_string-split result string)
    (let
      (
        (sub-position (substring? pattern string))
      )
      
      (if (not sub-position)
        (cons string result)
        (_string-split
          (cons
            (substring string 0 sub-position)
            result)
          (substring
            string
            (+ sub-position (string-length pattern))
            (string-length string)))))
  )

  (_string-split '() string)
)

#|
 | (get-environment-variables)  process context library procedure; r7rs p60
 |#
(define (get-environment-variables)
  (let*
    (
      (env (scream:java:lang:system ("getenv")))
      (keySet (env ("keySet")))
      (keyVector (keySet ("toArray")))
    )

    (vector->list (vector-map
      (lambda (key)
        (cons key (env ("get:java.lang.Object" key))))
      keyVector))
  )
)

;;
;; Time operations
;;

#|
 | (current-jiffy)  time library procedure; r7rs p60
 |#
(define (current-jiffy)
  (scream:java:lang:system ("currentTimeMillis")))

#|
 | (jiffies-per-second)  time library procedure; r7rs p60
 |#
(define (jiffies-per-second)
  1000)

#|
 | (current-second)  time library procedure; r7rs p60
 |#
(define (current-second)
  (/ (current-jiffy) (jiffies-per-second)))
