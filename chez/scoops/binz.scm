; from https://www.scheme.com/tspl2d/syntax.html
(define-syntax define-integrable
  (lambda (x)
    (define make-residual-name
      (lambda (name)
        (datum->syntax-object name
          (string->symbol
            (string-append "residual-"
              (symbol->string (syntax-object->datum name)))))))
    (syntax-case x (lambda)
      ((_ name (lambda formals form1 form2 ...))
       (identifier? (syntax name))
       (with-syntax ((xname (make-residual-name (syntax name))))
         (syntax
           (begin
             (define-syntax name
               (lambda (x)
                 (syntax-case x ()
                   (_ (identifier? x) (syntax xname))
                   ((_ arg (... ...))
                    (syntax
                      ((fluid-let-syntax
                         ((name (identifier-syntax xname)))
                         (lambda formals form1 form2 ...))
                       arg (... ...)))))))
             (define xname
               (fluid-let-syntax ((name (identifier-syntax xname)))
                 (lambda formals form1 form2 ...))))))))))
				 
