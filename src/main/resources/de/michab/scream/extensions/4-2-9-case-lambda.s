;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 2023 Michael G. Binz
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; r7rs definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | (case-lambda <clause> ...) case-lambda library syntax, r7rs 4.2.9 p21
 |#
(%syntax (case-lambda clause . clauses)
  (define clauses (cons clause clauses))

  (define (clause-to-line clause)
    `(list ,(length (car clause)) ,(cons 'lambda clause))
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
          (error "WRONG_NUMBER_OF_ARGUMENTS" length))
    )
  )

  (scream:eval (create-lambda))
)

#| Generated code.

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

    (define found (assv length a-list))

    (if found
      (apply (cadr found) args)
      (error "WRONG_NUMBER_OF_ARGUMENTS" length))
  )
) ; define range 

; (range 3)
; (range 3 5)
; (range 3 5 6) => error

|#
