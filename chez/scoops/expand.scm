
(define %sc-expand
  (lambda (exp)
    (letrec
;------!
     (
  (expand
   (lambda (x env)
     (cond ((atom? x)
	    (exp-atom x env))
	   ((macro? (car x))
	    (exp-macro x env))
	   (else
	    (expand2 x env)))))

  (exp-macro
   (lambda (x env)
     (let ((y (if (pair? macfun)
		  (cons (cdr macfun)(cdr x))	; alias
		  (macfun x)))) 		; macro
       (if (or (atom? y)
	       (equal? x y))
	   (expand2 y env)
	   (expand y env)))))

  (macfun '())

  (macro?
   (lambda (id)
     (set! macfun
	   (and (symbol? id)
		(or (getprop id 'pcs*macro))))
     macfun))

  (expand2
   (lambda (x env)
     (if (atom? x)
	 (exp-atom x env)
	 (case (car x)
	   ((QUOTE)	      x)
	   ((SET!)	      (exp-set! x env))
	   ((DEFINE)          (exp-define x env))
	   ((LAMBDA)	      (exp-lambda x env))
	   ((BEGIN IF)	      (exp-begin x env))
	   ((LETREC)	      (exp-letrec x env))
	   (else	      (exp-application x env))
	   ))))

  (exp-atom
   (lambda (x env)
     (if (or (not (symbol? x))
	     (memq x env)
	     (memq x '(#!true #!false
		       #!unassigned ))
	     (getprop x 'pcs*macro)
	     (getprop x 'pcs*primop-handler))
	 x
	 (list '%%get-scoops%% (list 'quote x)))))

  (exp-set!
   (lambda (x env)
     (pcs-chk-length= x x 3)
     (let ((id	(set!-id x))
	   (val (expand (set!-exp x) env)))
       (if (or (not (symbol? id))
	       (memq id env)
	       (memq id '(#!true #!false
			  #!unassigned ))
	       (getprop id 'pcs*macro)
	       (getprop id 'pcs*primop-handler))
	   (list 'SET! id val)
	   (list '%%set-scoops%% (list 'QUOTE id) val)))))

  (exp-define
   (lambda (x env)
     (pcs-chk-length= x x 3)
     (let ((op	(car x))	; define!, define
	   (id	(set!-id x))
	   (val (expand (set!-exp x) env)))
       (list op id val))))

  (exp-lambda
   (lambda (x env)
     (pcs-chk-length>= x x 3)
     (let ((bvl (lambda-bvl x)))
       (pcs-chk-bvl x bvl #!true)
       (cons 'LAMBDA
	     (cons bvl
		   (exp-args (lambda-body-list x)
			     '()
			     (extend env bvl)))))))

  (exp-begin
   (lambda (x env)
     (pcs-chk-length>= x x 1)
     (cons (car x)			; begin, if
	   (exp-args (cdr x) '() env))))

  (exp-letrec
   (lambda (x env)
     (pcs-chk-length>= x x 3)
     (let ((pairs (letrec-pairs x)))
       (pcs-chk-pairs x pairs)
       (let ((newenv  (extend env (mapcar car pairs))))
	 (cons 'LETREC
	       (cons (exp-pairs pairs '() newenv)
		     (exp-args (letrec-body-list x) '() newenv)))))))

  (exp-pairs
   (lambda (old new env)
     (if (null? old)
	 (reverse! new)
	 (let ((id  (caar old))
	       (exp (expand (cadar old) env)))
	   (exp-pairs (cdr old)
		      (cons (list id exp) new)
		      env)))))

  (exp-application
   (lambda (form env)
     (pcs-chk-length>= form form 1)
     (exp-args form '() env)))

  (exp-args
   (lambda (old new env)
     (if (null? old)
	 (reverse! new)
	 (exp-args (cdr old)
		   (cons (expand (car old) env) new)
		   env))))

  (extend
   (lambda (env bvl)
     (cond ((pair? bvl)
	    (extend (cons (car bvl) env) (cdr bvl)))
	   ((null? bvl)
	    env)
	   (else
	    (cons bvl env)))))

;------!
       )

     (expand exp '()))))



