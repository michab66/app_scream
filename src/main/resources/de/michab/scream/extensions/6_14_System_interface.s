; Scream @ https://github.com/urschleim/scream
;
; Copyright © 2024 Michael G. Binz

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
      (file (make-object (java.io.File filename)))
    )

    (if (not (file (exists)))
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
      ((scream:files:validate-exists dir) (list)))

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
      (file (make-object (java.io.File filename)))
    )
    
    (file (createNewFile)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; r7rs definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | (load filename)
 | (load filename environment-specifier)  file library procedure; r7rs p59
 |#
(define (load filename)
  (let
    (
      (evaluator ((make-object de.michab.scream.ScreamEvaluator) (EVAL)))
    )
    
    (evaluator (load filename (interaction-environment))))
)

#|
 | (file-exists? filename)  file library procedure; r7rs p60
 |#
(define (file-exists? filename)
  (let
    (
      (file (make-object (java.io.File filename)))
    )
    
    (file (exists)))
)

#|
 | (delete-file filename)  file library procedure; r7rs p60
 |#
(define (delete-file filename)
  ((scream:files:validate-exists filename) (delete)))

#|
 | (current-jiffy)  time library procedure; r7rs p60
 |#
(define (current-jiffy)
  ((make-object java.lang.System) (currentTimeMillis)))

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
