; Scream @ https://github.com/urschleim/scream
;
; Copyright © 2023 Michael G. Binz

;;
;; r7rs 4.2.9 p21
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scream definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; r7rs definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(define range
  (case-lambda
    ((e) (range 0 e))
    ((b e) (do ((r '() (cons e r))
                (e (- e 1) (- e 1)))
               ((< e b) r)))))

(range 3) =⇒ (0 1 2)
(range 3 5) =⇒ (3 4)
|#

(%syntax (case-lambda clause . clauses)
  (define clauses (cons clause clauses))

  (define (clause-to-line clause)
    `(list ,(length (car clause)) (lambda ,(car clause) ,(cadr clause)))
  )

  (define (clauses-to-lines clauses)
    (if (null? clauses)
      '()
      (cons 
        (clause-to-line (car clauses))
        (clauses-to-lines (cdr clauses))
      )
    )
  )

  (define (create-a-list clauses)
    `(define a-list ,(cons 'list (clauses-to-lines clauses)))
  )

  (define (create-lambda)
   `(lambda args
       ,(create-a-list clauses)
        (define length (length args))
        (define found (assv length a-list))
        (if found
          (apply (cadr found) args)
          (error "WRONG_NUMBER_OF_ARGUMENTS" 313 length))
    )
  )

  (scream:eval (create-lambda))
)

#|

(define range 

  (lambda args 

    (define a-list
      (list
        (list 
          1 
          (lambda (e) 
            (range 0 e)))

        (list
          2 
          (lambda (b e)
            (do ((r '() (cons e r))
                  (e (- e 1) (- e 1)))
                 ((< e b) r))))
      )
    )

    ; a-list = ((1 lambda1) (2 lambda))

    (define length (length args))
    (display "length: ")(display length)(newline)

    (define found (assv length a-list))
;    (display "found: ")(display found)(newline)
;    (display "         : ")(display (cadr found))(newline)

    (if found
      (apply (cadr found) args)
      (error "WRONG_NUMBER_OF_ARGUMENTS" 313 length))
  )
) ; define range 

; (range 3)
; (range 3 5)
; (range 3 5 6) => error

(define (range-test b e)
  (do ((r '() (cons e r))
        (e (- e 1) (- e 1)))
       ((< e b) r)))

|#
