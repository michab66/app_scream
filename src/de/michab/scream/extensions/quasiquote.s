; $Id: quasiquote.s 8 2008-09-14 14:23:20Z binzm $
;
; Scream / Runtime
;
; Released under Gnu Public License
; Copyright (c) 2002 Michael G. Binz
;
; The implementation of quasiquote and related stuff.  See R5RS 4.2.6, p. 13
; Note that replacement of
;  ` -> quasiqoute
;  , -> unquote
;  ,@ -> unquote-splicing
; is done in the scanner.
;
; 1282


;
; A stub giving a message if unquote is used outside an quasiquote expression.
;
(%syntax (unquote . rest)
  (error "ONLY_IN_QUASIQUOTE_CONTEXT"))



;
; A stub giving a message if unquote-splicing is used outside an quasiquote
; expression.
;
(%syntax (unquote-splicing . rest)
  (error "ONLY_IN_QUASIQUOTE_CONTEXT"))



;;
;; Quasiquote definition.  See R5RS 4.2.6 p.13
;;
(%syntax (quasiquote template)
  (letrec (

    (qq-application?
      (lambda (a)
        (and (list? a)
             (not (null? a))
             (= 2 (length a))
            ; (symbol? (car a))
             (memq (car a) '(quasiquote quote unquote unquote-splicing)))))

    (qq-vector
      (lambda (v lvl)
        (list->vector (qq-list (vector->list v) lvl))))

    (qq-list
      (lambda (argument lvl)
      ; TODO:  No checks are done in case the unquote and unquote-splicing
      ; expressions are syntactically wrong, e.g. have the wrong number of
      ; parameters.  In this case the unquotes are simply not evaluated.
        (cond
          ((null? argument)
           '())

          ((not (pair? argument))
           argument)

          ((qq-application? argument)
           (cond ((eq? 'unquote (car argument))
                   (evaluate (cadr argument)))
                 ((eq? 'quote (car argument))
                   (cons 'quote (qq-list (cdr argument) lvl)))
                 ((eq? 'unquote-splicing (car argument))
                   (evaluate (cadr argument)))))

          ((qq-application? (car argument))
            ; ...check for the special symbols 'unqoute and 'unqoute-splicing
            ; for additional evaluation.
            (cond ((eq? 'quasiquote (caar argument))
                    (cons (cons 'quasiquote
                                (qq-list (cdar argument) (+ lvl 1)))
                          (qq-list (cdr argument) lvl)))

                  ((eq? 'unquote (caar argument))
                    (if (eqv? lvl 1)
                      ; If this was unquote, eval the argument and construct
                      ; a new list element from the result.
                      (cons (evaluate (cadar argument))
                            (qq-list (cdr argument) lvl))
                      (cons (cons 'unquote (qq-list (cdar argument) (- lvl 1)))
                            (qq-list (cdr argument) lvl))))

                  ((eq? 'quote (caar argument))
                    (cons (cons 'quote (qq-list (cdar argument) lvl))
                          (qq-list (cdr argument) lvl)))

                  ((eq? 'unquote-splicing (caar argument))
                    (if (eqv? lvl 1)
                      ; For unqoute-splicing, the evaluation is expected to
                      ; return a list that is spliced into the original list.
                      (append (evaluate (cadar argument))
                              (qq-list (cdr argument) lvl))
                      (cons   (cons 'unquote-splicing
                                    (qq-list (cdar argument) (- lvl 1)))
                              (qq-list (cdr argument) lvl))))))

          ((vector? (car argument))
            (cons (qq-vector (car argument) lvl)
                  (qq-list (cdr argument) lvl)))

          (else
            (cons (qq-list (car argument) lvl)
                  (qq-list (cdr argument) lvl)))))))

  (cond ((pair? template)
         (qq-list template 1))
        ((vector? template)
         (qq-vector template 1))
        (else
          template))))
