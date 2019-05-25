;
; Following are a few macro definitions which implement constructs in other
; LISPs. They are not intended to be fully compatible to COMMON LISP or any
; other dialect, but are included as examples of how other constructs may
; be implemented, and how Scheme itself can be extended. Note also that the
; examples lack sufficient error checking - feel free to modify, extend,
; and add to any or all of macros for your own purposes.
; 

;
; CATCH/THROW - A catch form evaluates some subforms in such a way that, if
; a throw is executed during such evaluation, the evaluation is aborted at 
; that point and the catch form returns a value specified by the throw. The 
; catch/throw mechanism works even if the throw form is not within the lexical 
; scope of the catch. 
;
; The tags used for this implementation of catch/throw can be either symbols, 
; strings, or numbers. Note the use of fluids and continuations in this
; implementation.
; 

(macro catch		;(catch tag expression)
   (lambda (e)
      (let ((tag  (cadr e))
	    (form (caddr e)))
	   (cond ((string? tag)
		  (set! tag (string->symbol tag)))
		 ((number? tag)
		  (set! tag (implode (explode tag))))
		 ((and (pair? tag) (eq? (car tag) 'quote))
		  (set! tag (cadr tag))) )

	 `(call/cc (fluid-lambda (,tag) ,form)))))


(macro throw		;(throw tag value)
   (lambda (e)
      (let ((tag (cadr e))
	    (value (caddr e)))
	   (cond ((string? tag)
		  (set! tag (string->symbol tag)))
		 ((number? tag)
		  (set! tag (implode (explode tag))))
		 ((and (pair? tag) (eq? (car tag) 'quote))
		  (set! tag (cadr tag))) )

	   `(if (and (fluid-bound? ,tag)
		     (continuation? (fluid ,tag)))
	       ((fluid ,tag) ,value)
	       (error "Bad tag on throw" ,tag)))))

; 
; PROG - The prog construct allows one to write in a statement-oriented style 
; (ala FORTRAN), using go statements that can refer to tags in the body of the 
; prog. Modern LISP programming tends to use prog infrequently, however the 
; following exercise is a good example of how Scheme may be extended to take
; on characteristics of other LISPs.
;

(macro go
  (lambda (form)
     (if (integer? (cadr form))
        `(implode (explode ,(cadr form)))
     ;else
        (cdr form))))

(macro prog
  (lambda (form)
    (letrec 
      ((tagstart '())
       (buildvars
	 (lambda (proglist varlist)
           (if (null? proglist)
               varlist
               ;else
               (buildvars (cdr proglist)
                          (if (pair? (car proglist))
                              `(,(car proglist) ,@varlist)
                              ;else
                              `( (,(car proglist) '()) ,@varlist))))))
       (buildtags
	 (lambda (tbodys)
           (if (null? tagstart)
               tbodys
               ;else
               (buildtags 
                 `( ( ,(car tagstart) 
                       (lambda () ,@(getbody (cdr tagstart) '())))
                    ,@tbodys)))))
       (getbody 
	 (lambda (exprs body)
           (cond ((null? exprs)
                  (set! tagstart '())
                  (reverse! `((return ()) ,@body)))
                 ((or (symbol? (car exprs)) (integer? (car exprs)))
                  (set! tagstart
                        (if (integer? (car exprs))
			    `(,(implode (explode (car exprs))) ,@(cdr exprs))
                            ;else
			    exprs))
                  (reverse! `( (,(car tagstart)) ,@body)))
                 (else	
		   (getbody (cdr exprs) `(,(car exprs) ,@body)))))))
      
      (let ((letrec_body (getbody (cddr form) '()))
            (letrec_vars (reverse! (buildtags (buildvars (cadr form) '())))))
        
        `(call/cc (lambda (return)	
	            (letrec ,letrec_vars ,@letrec_body)))) )))	      
 
