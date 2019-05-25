
;      -*- Mode: Lisp -*-			       Filename:  edit.s

;                    Last Revision:  13-Sep-85 1230ct

;--------------------------------------------------------------------------;
;									   ;
;                         TI SCHEME -- PCS Compiler                        ;
;                  Copyright 1985 (c) Texas Instruments                    ;
;									   ;
;                              Paul Kristoff                               ;
;									   ;
;                       The Scheme Structure Editor                        ;
;									   ;
;--------------------------------------------------------------------------;


(define edit
  (letrec ((read-eval-print-loop
	    (letrec ((read-command
		       (lambda ()
			 (print 'EDIT->)
			 (set! buffer (read))
			 (if (atom? buffer)
			     (set! buffer (list (list buffer)))
			     (if (atom? (car buffer))
				 (set! buffer (list buffer))))))
		     (do-command
		       (lambda ()
			 (if (or (number? (car command))
				 (eq? (car command) '*))
			     (move (car command))
			     (case (car command)
				   ((?)  (print
					   (print-depth-length fp 2 10)))
				   ((P)  (print fp))
				   ((??) (pp
					   (print-depth-length fp 2 10)))
				   ((PP) (pp fp))
				   ((N)  (next))
				   ((PR) (previous))
				   ((B)  (beginning))
				   ((T)  (top))
				   ((F)  (find (cadr command)))
				   ((IB) (insert-before
					   (cadr command)
					   (caddr command)))
				   ((IA) (insert-after
					   (cadr command)
					   (caddr command)))
				   ((SB) (splice-before
					   (cadr command)
					   (caddr command)))
				   ((SA) (splice-after
					   (cadr command)
					   (caddr command)))
				   ((D)  (delete (cadr command)))
				   ((DP) (delete-parentheses
					   (cadr command)))
				   ((AP) (add-parentheses
					   (cadr command)
					   (caddr command)))
				   ((S)  (substitute
					   (cadr command)
					   (caddr command)))
				   ((R)  (replace
					   (cadr command)
					   (caddr command)))
				   ((PS) (ps))
				   ((MAC?) (mac? (cadr command)))
				   ((MAC) (create-ed-macro
					    (cadr command)
					    (caddr command)))
				   ((Q)  (set! done? t))
				   (else (if (ed-macro? (car command))
					     (expand-mac command)
					     (begin
					       (newline)
					       (set! buffer nil)
					       (writeln
						 "  ?  Unknown command: "
						 command))))
			      ))))
		     (mac?
		       (lambda (name)
			 (let ((temp (ed-macro? name)))
			   (if (null? temp)
			       (begin (writeln name " is not a macro.")
				      nil)
			       (pp (list 'mac (list name (car temp))
					      (cdr temp)))))))
		     (ed-macro?
		       (lambda (name)
			 (and (symbol? name)
			      (getprop name 'ed*macro))))
		     (expand-mac
		       (lambda (com)
			 (let* ((x (getprop (car com) 'ed*macro))
				(eem (expand-ed-macro
					(cdr com)
					(car x)
					(cdr x))))
			    (if (eq? eem 'error)
				(begin (set! buffer nil)
				       (writeln "  ?  Error with macro"
						command))
				(set! buffer
				      (append eem buffer))))))
		     (create-ed-macro
		       (lambda (name&nargs expan)
			 (putprop (car name&nargs)
				  (cons (cadr name&nargs)
					expan)
				  'ed*macro)))
		     (expand-ed-macro
			(lambda (args nargs expan)
			  (letrec
			    ((loop
			       (lambda (expan)
				 (cond ((null? expan) nil)
				       ((atom? expan)
					(let ((n (arg? expan)))
					  (if n
					      (list-ref args (-1+ n))
					      expan)))
				       ((atom? (car expan))
					 (let ((n (arg? (car expan))))
					   (cons (if n
						     (list-ref args
						       (-1+ n))
						     (car expan))
						 (loop (cdr expan)))))
				       (t (cons (loop (car expan))
						(loop (cdr expan)))))))
			     )
			    (if (= (length args) nargs)
				(loop expan)
				'error))))
		     )
	      (lambda ()
		(if (not (memq (car command) '(P ? PP ??)))
		    (print (print-depth-length fp 2 10)))
		(if (not done?)
		    (begin (read-command)
			   (do ()
			       ((null? buffer))
			       (set! command (car buffer))
			       (when (atom? command)
				 (set! command (list command)))
			       (set! buffer (cdr buffer))
			       (do-command))
			   (read-eval-print-loop))
		    (begin (top) fp)))))



;--------------------------------------------------------------------;
; MOVE								     ;
;  Argument: integer or *					     ;
;    Move repositions the fp to be the nth element of the current    ;
;    fp.  If an integer is positive the nth element will be from     ;
;    the left.	If the number is too large then the fp is moved to   ;
;    last element from the left.  If negative the nth element will   ;
;    be from the right.  If the absolute value of the number is      ;
;    larger than the number of elements in the fp, then the fp is    ;
;    repositioned to the 1st element from the left.  If the the      ;
;    argument is *, the fp is repositioned to be the cdr of the      ;
;    cons cell of the fp.					     ;
;--------------------------------------------------------------------;

	   (move
	     (let ((stop (lambda ()
			   (newline)
			   (writeln "  ?  Cannot do a Move on an atom."))))
	       (lambda (n)
		 (cond ((atom? fp) (stop))
		       ((eq? n '*)
			(begin (push fp '*)
			       (set! fp (cdr (last-pair fp)))
			       fp))
		       (t (let ((num (correct-position n)))
			    (cond ((null? n) (circular num))
				  ((<= num 0) (push fp 1)
					      (set! fp (car fp)))
				  (t (let ((smart-list
					     (smart-list-ref
						fp (-1+ num))))
				       (push fp
					     (- num (cdr smart-list)))
				       (set! fp (car smart-list))
				       fp)))))))))

;--------------------------------------------------------------------;
;  BEGINNING							     ;
;   No arguments						     ;
;   Repositions the fp to be the parent of the current fp	     ;
;--------------------------------------------------------------------;
	   (beginning
	     (let ((stop (lambda ()
			   (newline)
			   (writeln "  ?  Already at top level."))))
	       (lambda ()
		 (if (at-top-level?)
		     (stop)
		   (let ((stack-frame (pop)))
		     (set! fp (fp-part stack-frame))
		     fp)))))

;--------------------------------------------------------------------;
;  NEXT 							     ;
;  No Arguments							     ;
;  Moves the fp to be the next element to the right of the parent    ;
;  of the current fp.  If the fp is pointing to the last element,    ;
;  the fp remains the same.					     ;
;--------------------------------------------------------------------;

	   (next
	     (let ((stop (lambda ()
			   (newline)
			   (writeln
			       "  ?  There is no Next from this position")))
		   (stop1
		     (lambda ()
		       (newline)
		       (writeln
			 "  ?  Can't execute Next command at top level"))))
	       (lambda ()
		 (if (at-top-level?)
		     (stop1)
		   (let ((stack-frame (pop)))
		     (set! fp (fp-part stack-frame))
		     (move (if (eq? (element-part stack-frame) '*)
			       (begin (stop) '*)
			     (1+ (element-part stack-frame))))
		     fp)))))

;--------------------------------------------------------------------;
;  PREVIOUS							     ;
;  No Arguments 						     ;
;  Repositions the fp to be the previous element of the parent of    ;
;  the current fp.  If already at the first element of the fp, then  ;
;  the fp remains the same.					     ;
;--------------------------------------------------------------------;
	   (previous
	     (let ((stop (lambda ()
			   (newline)
			   (writeln
			     "  ?  There is no Previous from this position")))
		   (stop1 (lambda ()
			    (newline)
			    (writeln
			      "  ?  Can't execute Previous at top level"))))
	       (lambda ()
		 (if (at-top-level?)
		     (stop1)
		   (let ((stack-frame (pop)))
		     (set! fp (fp-part stack-frame))
		     (move (cond ((eq? (element-part stack-frame) '*)
				  (begin (stop) '*))
				 ((= (element-part stack-frame) 1) (stop) 1)
				 (t (-1+ (element-part stack-frame)))))
		     fp)))))

;--------------------------------------------------------------------;
;  TOP								     ;
;  No arguments 						     ;
;  Sets the fp to point to the car of very-top.  Resets the stack.   ;
;--------------------------------------------------------------------;
	   (top
	     (lambda ()
	       (set! fp (car very-top))
	       (set! stack initial-stack)
	       ))
;--------------------------------------------------------------------;
;   FIND							     ;
;   Can take an argument					     ;
;   Searches beginning with the FP (not including the FP) until the  ;
;   it either finds the pfv (using equal?) or the whole stack is     ;
;   popped.  If it is found the FP is moved to that point.  If is    ;
;   it is not the FP and STACK remain the same.  The value maybe     ;
;   inside the FP.						     ;
;--------------------------------------------------------------------;
	   (find
	     (letrec ((find-next
			(lambda ()
			  (cond ((equal? fp pfv) (set! found? t))
				((atom? fp) (get-next-element))
				(t (move 1)
				   (find-next)))))
		      (get-next-element
			(let ((stop (lambda ()
				      (newline)
				      (writeln "  ?  Did not find "
					       pfv))))
			  (lambda ()
			    (if (at-top-level?)
				(stop)
			      (let ((stack-frame (pop)))
				(let ((tfp (fp-part stack-frame))
				      (tel (element-part
					     stack-frame)))
				  (if (eq? tel '*)
				      (get-next-element)
				      (let ((next-element
					      (list-ref-* tfp tel)))
					(push tfp
					      (if (eq? (cdr next-element)
						       '*)
						  '*
						  (1+ tel)))
					(set! fp
					      (car next-element))
					(find-next)))
				  ))))))
		      (temp-stack nil)
		      (temp-fp nil)
		      (found? nil)
		      (pfv '**unbound**)
		      )
	       (lambda v
		 (if (not (null? (car v)))
		     (set! pfv (car v)))
		 (set! found? nil)
		 (set! temp-stack stack)
		 (set! temp-fp fp)
		 (if (atom? fp) 		; allows find next if fp is
		     (get-next-element) 	; equal to the pfv
		     (begin (move 1) (find-next)))
		 (if (not found?)
		     (let ((par (parent stack)))
		       (set! stack temp-stack)
		       (set! fp temp-fp)))
		 fp)))
;--------------------------------------------------------------------;
;  REPLACE							     ;
;  arguments n:  The element being replaced (nth element of the FP). ;
;	     v:  The value the nth element will replace.	     ;
;  Replace will replace the nth element of the FP with v.  n can be  ;
;  either negative or positive.  If too large an error is indicated. ;
;--------------------------------------------------------------------;
	   (replace
	     (lambda (n v)
	       (cond ((eq? n '*) (set-cdr! (last-pair fp) v))
		     ((not (number? n))
		      (newline)
		      (writeln "  ?  Non-number or non-* to Replace: " n))
		     ((= n 0) (correct-stack v)
		      (set! fp v))
		     (t (let ((num (correct-position n)))
			  (if (null? num)
			      (circular-error n)
			      (let ((sc (smart-list-tail
					  fp
					  (-1+ num))))
				(if (atom? sc)
				    (not-enough-elements-error n)
				    (set-car! sc v)))))))))
;--------------------------------------------------------------------;
;  SUBSTITUTE							     ;
;  arguments for :  The value searched for.			     ;
;	     this:  The value that replaces the value searched for   ;
;  Searches the FP for 'for'.  It replaces all occurrences of 'for'  ;
;  with 'this'.  If none are found it will indicate that.            ;
;--------------------------------------------------------------------;
	   (substitute
	     (lambda (for this)
	       (letrec ((found? nil)
			(subst
			  (lambda (l)
			    (cond ((null? l) nil)
				  ((equal? for l) (set! found? t) this)
				  ((atom? l) l)
				  (t (cons (subst (car l))
					   (subst (cdr l)))))))
			)
		  (set! fp (subst fp))
		  (if (not found?)
		      (begin (newline)
			     (writeln "  ?  Can't find " for))
		      (correct-stack fp))
		  fp)))
	   (delete
	     (lambda (n)
	       (cond ((eq? n '*) (set-cdr! (last-pair fp) nil))
		     ((not (number? n))
		      (newline)
		      (writeln "  ?  Non-number or non-* to Delete: " n))
		     ((zero? n) (set! fp nil) (correct-stack fp))
		     (t (let ((num (correct-position n)))
			  (cond ((null? num) (circular-error n))
				((atom? fp)
				 (newline)
				 (writeln
				   "  ?  FP is an atom, can't delete "
				   n " element"))
				((= num 1)
				  (set! fp (cdr fp))
				  (correct-stack fp))
				(t (let ((sc (smart-list-tail fp (- num 2)))
					 (scc (smart-list-tail fp num)))
				     (if (and (atom? scc)
					      (not (null? scc))) ;PRK 53085
					 (not-enough-elements-error n)
					 (set-cdr! sc scc))))))))))
;--------------------------------------------------------------------;
;  DELETE PARENTHESES						     ;
;  argument n:	The nth element of the FP			     ;
;  Deletes the parentheses from around the nth element of the FP.    ;
;  The nth element must be a list otherwise an error will occur.  n  ;
;  maybe either negative or positive.				     ;
;--------------------------------------------------------------------;
	   (delete-parentheses
	     (lambda (n)
	       (letrec ((stop1
			  (lambda ()
			    (newline)
			    (writeln
			     "  ?  Can't delete parentheses for this position "
			      n)))
			(stop2 (lambda ()
				 (newline)
				 (writeln "  ?  Element is not a list")))
			)
		  (if (and (number? n) (not (zero? n)))
		      (let* ((num (correct-position n)))
			(if (null? num)
			    (circular-error n)
			    (let ((elem (smart-list-ref fp (-1+ num)))
				  (next-elem (smart-list-tail fp num))
				  )
			      (when (eq? next-elem '*atom-returned*)
				(set! next-elem '()))
			      (cond ((atom? fp)
				     (newline)
				     (writeln
				       "  ?  FP is an atom, can't delete "
				       n " element."))
				    ((not (zero? (cdr elem)))
				     (not-enough-elements-error n))
				    ((not (list? (car elem)))
				     (stop2))
				    ((= num 1)
				     (set! fp (append! (car elem) next-elem))
				     (correct-stack fp))
				    (t (set-cdr! (list-tail fp (- num 2))
					 (append! (car elem) next-elem)))))))
		      (stop1))
		  )))
;--------------------------------------------------------------------;
;  ADD PARENTHESES						     ;
;  arguments x:  One or two arguments				     ;
;  Will add parentheses from the first argument to the second	     ;
;  argument (left to right).  The first argument must be to the left ;
;  or the same as the second argument.	If the first argument is * or;
;  0 (zero) the second argument is ignored.			     ;
;--------------------------------------------------------------------;
	   (add-parentheses
	     (lambda x
	       (let ((m (car x))(n (cadr x)))
		 (cond ((atom? fp)
			(newline)
			(writeln
			  "  ?  FP is an atom, can't Add Parentheses"))
		       ((eq? m '*)
			(let ((lp (last-pair fp)))
			  (set-cdr! lp (list (cdr lp)))))
		       ((not (number? m))
			(newline)
			(writeln
			    "  ?  Non-number or non-* to Add Parentheses:  "
			    m))
		       ((= m 0) (set! fp (cons fp nil))
			(correct-stack fp))
		       ((eq? n '*)
			(let ((cm (correct-position m)))
			  (cond ((null? cm)(circular-error m))
				((= cm 1) (set! fp (cons fp nil))
				 (correct-stack fp))
				(t (let ((slt1
					   (smart-list-tail fp (- cm 2)))
					 (slt2
					   (smart-list-tail fp (-1+ cm))))
				     (if (atom? slt2)
					 (not-enough-elements-error m)
				       (set-cdr! slt1
						 (cons slt2 nil))))))))
		       ((not (number? n))
			(newline)
			(writeln
			    "  ?  Non-number or non-* to Add Parentheses: "
			    n))
		       (t (let ((cm (correct-position m))
				(cn (correct-position n)))
			    (cond ((null? cm) (circular-error m))
				  ((null? cn) (circular-error n))
				  ((<= cm 0) (not-enough-elements-error m))
				  ((<= cn 0) (not-enough-elements-error n))
				  ((> cm cn)
				   (newline)
				   (writeln
				     "  ?  First argument, " m
		    " is positioned to the right of the 2nd, " n))
				  (t (let ((end-fp (list-tail fp  cn))
					   (last-arg-tail
					     (smart-list-tail fp (-1+ cn))))
				       (if (atom? last-arg-tail)
					   (not-enough-elements-error n)
					 (begin (set-cdr! last-arg-tail nil)
						(if (= cm 1)
						    (begin
						      (set! fp
							    (cons fp end-fp))
						      (correct-stack fp))
						  (set-cdr!
						    (list-tail fp (- cm 2))
						    (cons
						      (list-tail fp (-1+ cm))
						      end-fp))))))))))
		       ))))
;--------------------------------------------------------------------;
;  SPLICE BEFORE						     ;
;  arguments n:  The nth element of the FP			     ;
;	     v:  The list of values to be spliced before the nth     ;
;		 element.					     ;
;  Splices before the nth element of the FP, the elements in v.  If  ;
;  v is not a list an error is indicated.			     ;
;--------------------------------------------------------------------;
	   (splice-before
	     (lambda (n v)
	       (cond ((atom? fp)
		      (newline)
		      (writeln
			"  ?  FP is an atom, can't splice before "
			n " element"))
		     ((or (not (number? n)) (zero? n))
		      (newline)
		      (writeln
		          "  ?  First argument must be a non-zero integer: "
			       n))
		     ((not (list? v))
		      (newline)
		      (writeln "  ?  Second argument must be a list: " v))
		     (t (let ((num (correct-position n)))
			  (cond ((null? num)
				 (circular-error n))
				((= num 1)
				 (set! fp (append! v fp))
				 (correct-stack fp))
				(t (let ((slt1
					   (smart-list-tail fp (- num 2)))
					 (slt2
					   (smart-list-tail fp (-1+ num))))
				     (if (atom? slt2)
					 (not-enough-elements-error n)
					 (set-cdr! slt1
						   (append! v slt2))))))))
		       )))
;--------------------------------------------------------------------;
;  SPLICE AFTER 						     ;
;  arguments n:  The nth element of the FP.			     ;
;	     v:  The list of elements that are splice after the nth  ;
;		 element.					     ;
;  The elements of v are placed after the nth element of the FP.  If ;
;  v is not a list an error is indicated.			     ;
;--------------------------------------------------------------------;
	   (splice-after
	     (lambda (n v)
	       (cond ((atom? fp)
		      (newline)
		      (writeln
			"  ?  FP is an atom, can't splice after "
			n " element"))
		     ((or (not (number? n)) (zero? n))
		      (newline)
		      (writeln
		          "  ?  First argument must be a non-zero integer: "
			  n))
		     ((not (list? v))
		      (newline)
		      (writeln "  ?  Second argument must be a list: " v))
		     (t (let ((num (correct-position n)))
			  (if (null? num)
			      (circular-error n)
			      (let ((slt1 (smart-list-tail fp (-1+ num)))
				    (slt2 (smart-list-tail fp num)))
				(if (atom? slt1)
				    (not-enough-elements-error n)
				    (set-cdr! slt1
					      (append! v slt2)))))))
		     )))
;--------------------------------------------------------------------;
;  INSERT BEFORE						     ;
;  arguments num:  The nth element of the FP			     ;
;	     v	:  The value being placed before the nth element     ;
;  Makes sure that the v can be inserted the calls splice-before     ;
;  with num and (list v).					     ;
;--------------------------------------------------------------------;
	   (insert-before
	     (lambda (num v)
	       (cond ((atom? fp)
		      (newline)
		      (writeln
			"  ?  FP is an atom, can't insert before "
			n " element"))
		     (t (splice-before num (cons v nil))))))
;--------------------------------------------------------------------;
;  INSERT AFTER 						     ;
;  arguments num:  The nth element of the FP			     ;
;	     v	:  The value being placed after the nth element      ;
;  Makes sure that the v can be inserted the calls splice-after      ;
;  with num and (list v).					     ;
;--------------------------------------------------------------------;
	   (insert-after
	     (lambda (num v)
	       (cond ((atom? fp)
		      (newline)
		      (writeln
			"  ?  FP is an atom, can't insert after "
			n " element"))
		     (t (splice-after num (cons v nil))))))
;--------------------------------------------------------------------;
;								     ;
;			   Help Functions			     ;
;								     ;
;--------------------------------------------------------------------;

	   (push
	     (lambda (l pos)
	       (set! stack (cons (list* l pos) stack))))

	   (pop
	     (lambda ()
	       (if (null? (cdr stack))
		   'cannot-pop-stack
		 (begin0 (car stack)
			 (set! stack (cdr stack))))))

	   (fp-part car)

	   (element-part cdr)
	  ;----------------------------------------------------------;
	  ;  Print depth length 				     ;
	  ;  It will return a list with depth of print-level and     ;
	  ;  length of print-length.  It will replace all levels     ;
	  ;  lower than print-level with # and all elements further  ;
	  ;  than print-length with ... 			     ;
	  ;----------------------------------------------------------;

	   (print-depth-length
	     (letrec ((p1 0)
		      (loop
			(lambda (l lev len)
			  (cond ((<= len 0) '(...))
				((atom? l) l)
				((<= lev 0) '#\#)
				((atom? (car l))
				 (cons (car l)
				       (loop (cdr l) lev (-1+ len))))
				(t (cons (loop (car l) (-1+ lev) p1)
					 (loop (cdr l) lev (-1+ len)))))))
		      )
	       (lambda (l print-level print-length)
		 (set! p1 print-length)
		 (loop l print-level print-length) )))

	   (list-length 	; Gives list-length while checking for
	     (lambda (l)	       ; circular lists. Returns nil
	       (letrec ((loop (lambda ()  ; if circular list is found
				(cond ((atom? fast) n)
				      ((atom? (cdr fast)) (+ n 1))
				      ((and (eq? fast slow) (> n 0)) nil)
				      (t (set! fast (cddr fast))
					 (set! slow (cdr slow))
					 (set! n (+ n 2))
					 (loop)))))
		  (n 0)
		  (fast l)
		  (slow l))
		       (loop))))

	   (correct-position	; If number is negative, translates it
	     (lambda (n)	       ; the equivalent positive number.
	       (if (< n 0)
		   (+ (list-length fp) (1+ n))
		   n)))

	  ;----------------------------------------------------------;
	  ;  Smart-list-ref					     ;
	  ;  Returns a pair.  The first of which is the list-ref of  ;
	  ;  l.  The second is the number left over.  This number    ;
	  ;  will be zero unless the number is larger than the number;
	  ;  of elements in the list.  Then it will show the number  ;
	  ;  left and return the last element.			     ;
	  ;----------------------------------------------------------;
	   (smart-list-ref
	     (lambda (l n)
	       (cond ((atom? l) nil)
		     ((atom? (cdr l)) (cons (car l) n))
		     ((zero? n) (cons (car l) 0))
		     (t (smart-list-ref (cdr l) (-1+ n))))))

	   (at-top-level?
	     (lambda () (null? (cdr stack))))
	  ;----------------------------------------------------------;
	  ;  Correct-stack					     ;
	  ;  Corrects the parent of the FP when the FP is changed    ;
	  ;  with a set! instead of set-car! or set-cdr!	     ;
	  ;----------------------------------------------------------;

	   (correct-stack
	     (lambda (l)
	       (let ((par (parent stack)))
		 (if (eq? (element-part par) '*)
		     (if (atom? l)
			 (set-cdr! (last-pair (fp-part par)) l)
			 (let ((stack-frame (pop)))
			   (set! fp (fp-part stack-frame))
			   (set-cdr! (last-pair fp) l)))
		     (set-car! (if (= (element-part par) 1)
				   (fp-part par)
				   (list-tail (fp-part par)
					      (-1+ (element-part par))))
			       l)))))

	   (list?
	     (lambda (l)
	       (and (pair? l)
		    (null? (cdr (last-pair l))))))

	  ;----------------------------------------------------------;
	  ;  List-ref-* 					     ;
	  ;  Used in Find.  It is set up to know about the *th	     ;
	  ;  position.	It counts the * as another element.  Other   ;
	  ;  than this, it is just like smart-list-ref. 	     ;
	  ;----------------------------------------------------------;
	   (list-ref-*
	     (lambda (l n)
	       (cond ((atom? l) (cons l '*))
		     ((zero? n) (cons (car l) 0))
		     (t (list-ref-* (cdr l) (-1+ n))))))

	   (parent car)

	  ;----------------------------------------------------------;
	  ;  Smart-list-tail					     ;
	  ;  This is used in the modifying commands.  It allows the  ;
	  ;  calling function to figure out if there is an nth	     ;
	  ;  element.  An atom is returned if it there are not n     ;
	  ;  elements.	The value of this command is used in set-car!;
	  ;  and set-cdr!.  Thus it cannot be an atom.		     ;
	  ;----------------------------------------------------------;
	   (smart-list-tail
	     (letrec ((loop
			(lambda (l n)
			  (cond ((zero? n) l)
				((atom? l) '**atom-returned**) ;PRK 53085
				(t (loop (cdr l) (-1+ n)))))))
	       (lambda (l n)
		 (if (< n 0)
		     '**atom-returned**
		     (loop l n)))))

	   (not-enough-elements-error
	     (lambda (n)
	       (newline)
	       (writeln "  ?  There are not " n " elements")))

	   (circular-error
	     (lambda (n)
	       (newline)
	       (writeln
		   "  ?  FP is a circular list, can't use negative numbers: "
		   n)))

	   (arg?
	     (lambda (a)
	       (let ((x (explode a)))
		 (if (eq? (car x) '#\#)
		     (if (number-range? (cdr x))
			 (symbols->number (cdr x) 10 0)
			 #!false)
		     #!false))))

	   (number-range?
	     (lambda (l)
	       (if (null? l)
		   #!true
		   (let ((a (symbol->ascii (car l))))
		     (if (and (> a 47) (< a 58))
			 (number-range? (cdr l))
			 #!false)))))

	   (symbols->number
	     (lambda (l b n)
	       (if (null? l)
		   0
		   (+ (symbols->number (cdr l) b (1+ n))
		      (* (expt b n)
			 (- (symbol->ascii (car l)) 48))))))

;--------------------------------------------------------------------;
;								     ;
;			     Variables				     ;
;								     ;
;--------------------------------------------------------------------;

	   (very-top nil)
	   (initial-stack nil)
	   (fp nil)
	   (stack nil)
	   (command nil)
	   (done? nil)
	   (buffer nil)


;--------------------------------------------------------------------;
;								     ;
;			 Debugging Functions			     ;
;								     ;
;--------------------------------------------------------------------;

	   (ps (lambda () (print (print-depth-length stack 4 10))))


	   )

    (lambda (l)
      (set! done? nil)
      (set! fp l)
      (set! very-top (list fp))
      (set! initial-stack (list (list* very-top 1)))
      (set! stack initial-stack)
      (read-eval-print-loop))))

