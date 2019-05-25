;;; extend.s

;;; Copyright (c) 1986 R. Kent Dybvig
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this 
;;; software must include this copyright notice in full.

;;;  
;;; EXTEND-SYNTAX is a syntax extension facility based on pattern match-
;;; ing. The extend-syntax code presented here was contributed by R. Kent
;;; Dybvig, as implemented for Chez Scheme and described in his book,
;;; The Scheme Programming Language. The code has been modified to run 
;;; under TI Scheme.
;;;
;;; Methods similar to extend-syntax exist in most implementations of 
;;; Scheme, including TI Scheme's own SYNTAX special form. EXTEND-SYNTAX
;;; however, is much more powerful in its capabilities than SYNTAX. A full
;;; description of extend-syntax is beyond the scope of this documentation.
;;; Other than some examples I will list here, I must refer you to Kent's 
;;; book or other documents for further information on EXTEND-SYNTAX. For 
;;; those of you already familiar with extend-syntax, its basic syntax is:
;;;
;;;  (extend-syntax (name key ...) (pattern optional-fender expansion) ...)
;;;
;;; Examples:
;;;
;;;     (extend-syntax (when)		     
;;;       ((when test exp1 exp2 ...)	     
;;;	   (if test (begin exp1 exp2 ...) #F)))
;;;
;;;     (extend-syntax (let)
;;;	  ((let ((x v) ...) e1 e2 ...)
;;;	   ((lambda (x ...) e1 e2 ...) v ...)))
;;;
;;;
;;;  NOTE - You may use EXPAND to see an expansion of an extend-syntax
;;; 	    definition. See the READ.ME file for explanation of EXPAND.
;;;


(macro unless 
   (lambda (e) 
      (append (list 'when (list 'not (cadr e))) (cddr e))))

(define-structure %%boxed-obj value)

(define box (lambda (objct) (make-%%boxed-obj 'value objct)))

(define unbox (lambda (box) (if (%%boxed-obj? box)
				(%%boxed-obj-value box)
				(error "Object referenced is not a BOX" box))))

(define set-box! (lambda (box objct)
		   (if (%%boxed-obj? box)
		       (set! (%%boxed-obj-value box) objct)
		       (error "Object to be set is not a BOX" box))))


(define %%map2
  (lambda (f a1 a2)
    (let loop ((result ())
	       (a1 a1)
	       (a2 a2))
      (if (null? a1)
	  (reverse! result)
	  (loop (cons (f (car a1) (car a2)) result)
		(cdr a1)
		(cdr a2))))))

(macro %%multi-mapper
  (lambda (x)
    (cond ((syntax-match? '(%%multi-mapper) '(%%multi-mapper f a1 ...) x)
	   (let ((g10 (map (lambda (x) (gensym))
			   (cddr x))))
	     (quasiquote (let loop ((result ())
				    (unquote-splicing
				     (%%map2  (lambda (g9 g11)
						(quasiquote ((unquote g11) 
							     (unquote g9))))
					      (cddr X) g10)))
			   (if (or (unquote-splicing
				    (map (lambda (g11)
					   (quasiquote 
					      (null? (car (unquote g11)))))
					 g10)))
			       (reverse! result)
			       (loop (cons ((unquote (cadr x))
					    (unquote-splicing
					     (map (lambda (g11)
						    (quasiquote 
							(car (unquote g11))))
						  g10)))
					   result)
				     (unquote-splicing
				      (map (lambda (g11)
					     (quasiquote (cdr (unquote g11))))
					   g10))))))))
	  (else (error "%%MULTI-MAPPER: invalid syntax " x)))))


(define %%make-syntax 
 (letrec
   ((id-name car)
    (id (lambda (name accessor control) (list name accessor control)))
    (id-accessor cadr)
    (id-control caddr)
    (loop (lambda () (box '())))
    (loop-ids unbox)
    (loop-ids! set-box!)
    (c...rs
       `((car caar . cdar)
         (cdr cadr . cddr)
         (caar caaar . cdaar)
         (cadr caadr . cdadr)
         (cdar cadar . cddar)
         (cddr caddr . cdddr)
         (caaar caaaar . cdaaar)
         (caadr caaadr . cdaadr)
         (cadar caadar . cdadar)
         (caddr caaddr . cdaddr)
         (cdaar cadaar . cddaar)
         (cdadr cadadr . cddadr)
         (cddar caddar . cdddar)
         (cdddr cadddr . cddddr)))
    (add-car
       (lambda (accessor)
          (let ((x (and (pair? accessor) (assq (car accessor) c...rs))))
             (if (null? x)
                 `(car ,accessor)
                 `(,(cadr x) ,@(cdr accessor))))))
    (add-cdr
       (lambda (accessor)
          (let ((x (and (pair? accessor) (assq (car accessor) c...rs))))
             (if (null? x)
                 `(cdr ,accessor)
                 `(,(cddr x) ,@(cdr accessor))))))
    (parse
       (lambda (keys pat acc cntl)
          (cond
             ((symbol? pat)
              (if (memq pat keys)
                  '()
                  (list (id pat acc cntl))))
             ((pair? pat)
              (if (equal? (cdr pat) '(...))
                  (let ((x (gensym)))
                     (parse keys (car pat) x (id x acc cntl)))
                  (append (parse keys (car pat) (add-car acc) cntl)
                          (parse keys (cdr pat) (add-cdr acc) cntl))))
             (else '()))))

    (gen
       (lambda (exp ids loops)
          (cond
             ((symbol? exp)
              (let ((id (lookup exp ids)))
                 (if (null? id)
                     exp
                     (begin
                        (add-control! (id-control id) loops)
                        (list 'unquote (id-accessor id))))))
             ((pair? exp)
              (cond
                 ((eq? (car exp) 'with)
                  (unless (syntax-match? '(with) '(with ((p x) ...) e ...) exp)
                     (error  "EXTEND-SYNTAX: invalid 'with' form" exp))
                  (list 'unquote
                     (gen-with
                        (map car (cadr exp))
                        (map cadr (cadr exp))
                        (caddr exp)
                        ids
                        loops)))
                 ((and (pair? (cdr exp)) (eq? (cadr exp) '...))
                  (let ((x (loop)))
                     (make-loop
                        x
                        (gen (car exp) ids (cons x loops))
                        (gen (cddr exp) ids loops))))
                 (else
                  (let ((a (gen (car exp) ids loops))
                        (d (gen (cdr exp) ids loops)))
                     (if (and (pair? d) (eq? (car d) 'unquote))
                         (list a (list 'unquote-splicing (cadr d)))
                         (cons a d))))))
             (else exp))))

    (gen-with
       (lambda (pats exps body ids loops)
          (if (null? pats)
              (make-quasi (gen body ids loops))
              (let ((p (car pats)) (e (car exps)) (g (gensym)))
                 `(let ((,g ,(gen-quotes e ids loops)))
                     ,(gen-with
                         (cdr pats)
                         (cdr exps)
                         body
                         (append (parse '() p g '()) ids)
                         loops))))))

    (gen-quotes
       (lambda (exp ids loops)
          (cond
             ((syntax-match? '(quote) '(quote x) exp)
              (make-quasi (gen (cadr exp) ids loops)))
             ((pair? exp)
              (cons (gen-quotes (car exp) ids loops)
                    (gen-quotes (cdr exp) ids loops)))
             (else exp))))

    (lookup
       (lambda (sym ids)
          (let ((x (mem (lambda (x) (eq? (id-name x) sym)) ids)))
              (and x (car x)))))

    (add-control!
       (lambda (id loops)
          (unless (null? id)
             (when (null? loops)
                (error "EXTEND-SYNTAX: missing ellipsis in expansion"))
             (let ((x (loop-ids (car loops))))
                (unless (memq id x)
                   (loop-ids! (car loops) (cons id x))))
             (add-control! (id-control id) (cdr loops)))))

    (make-loop
       (lambda (loop body tail)
          (let ((ids (loop-ids loop)))
             (when (null? ids)
                (error "EXTEND-SYNTAX: extra ellipsis in expansion"))
             (cond
                ((equal? body (list 'unquote (id-name (car ids))))
                 (if (null? tail)
                     (list 'unquote (id-accessor (car ids)))
                     (cons (list 'unquote-splicing (id-accessor (car ids)))
                           tail)))
                ((and (null? (cdr ids))
                      (syntax-match? '(unquote) '(unquote (f x)) body)
                      (eq? (cadadr body) (id-name (car ids))))
                 (let ((x `(%%multi-mapper ,(caadr body) ,(id-accessor (car ids)))))
                    (if (null? tail)
                        (list 'unquote x)
                        (cons (list 'unquote-splicing x) tail))))
                (else
                 (let ((x `(%%multi-mapper (lambda ,(map id-name ids) ,(make-quasi body))
                                ,@(map id-accessor ids))))
                    (if (null? tail)
                        (list 'unquote x)
                        (cons (list 'unquote-splicing x) tail))))))))

    (make-quasi
       (lambda (exp)
          (if (and (pair? exp) (eq? (car exp) 'unquote))
              (cadr exp)
              (list 'quasiquote exp))))
    
    (make-clause
       (lambda (ks cl x)
          (cond
             ((syntax-match? '() '(pat fender exp) cl)
              (let ((pat (car cl)) (fender (cadr cl)) (exp (caddr cl)))
                 (let ((ids (parse ks pat x '())))
                    `((and (syntax-match? ',ks ',pat ,x)
                           ,(gen-quotes fender ids '()))
                      ,(make-quasi (gen exp ids '()))))))
             ((syntax-match? '() '(pat exp) cl)
              (let ((pat (car cl)) (exp (cadr cl)))
                (let ((ids (parse ks pat x '() )))
                    `((syntax-match? ',ks ',pat ,x)
                      ,(make-quasi (gen exp ids '()))))))
             (else
              (error  "EXTEND-SYNTAX: invalid clause" cl)))))
    (make-syntaxer
       (let ((x (string->uninterned-symbol "x")))
          (lambda (keys clauses)
             `(lambda (,x)
                 (cond
		  ,@(map (lambda (cl)
			   (make-clause keys cl x)) clauses)
		    (else
		     (error (string-append (symbol->string ',(car keys))
					   ": invalid syntax") ,x))))))))
   make-syntaxer))

(define mem
  (lambda (f alist)
    (let loop ((l alist))
      (if (null? l)
	  '()
	  (if (f (car l))
	      l
	      (loop (cdr l)))))))

;  (define-syntax-expander extend-syntax              ;Original code in body of letrec
;     (lambda (x e)
;        (let ((keys (cadr x)) (clauses (cddr x)))
;           (e `(define-syntax-expander ,(car keys)
;                  ,(make-syntax keys clauses))))))



(macro extend-syntax
 (lambda (x)
    (let ((keys (cadr x))
	  (clauses (cddr x)))
      `(macro ,(car keys) ,(%%make-syntax keys clauses)))))


;   (define-syntax-expander extend-syntax/code         ;original code in body of letrec
;      (lambda (x e)
;         (let ((keys (cadr x)) (clauses (cddr x)))
;            `',(make-syntax keys clauses)))))

(macro extend-syntax/code
  (lambda (x)
    (let ((keys (cadr x)) (clauses (cddr x)))
     `',(%%make-syntax keys clauses))))

;;; syntax-match? is used by extend-syntax to choose among clauses and
;;; to check for syntactic errors.  It is also available to the user.

(define syntax-match?
   (lambda (keys pat exp)
      (cond
         ((symbol? pat) (if (memq pat keys) (eq? exp pat) #!true))
         ((pair? pat)
          (if (equal? (cdr pat) '(...))
              (let f ((lst exp))
                 (or (null? lst)
                     (and (pair? lst)
                          (syntax-match? keys (car pat) (car lst))
                          (f (cdr lst)))))
              (and (pair? exp)
                   (syntax-match? keys (car pat) (car exp))
                   (syntax-match? keys (cdr pat) (cdr exp)))))
         (else (equal? exp pat)))))

 
