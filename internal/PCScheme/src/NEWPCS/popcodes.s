
;      -*- Mode: Lisp -*-			    Filename:  popcodes.s

;--------------------------------------------------------------------------;
;									   ;
;			  TI SCHEME -- PCS Compiler			   ;
;		   Copyright 1985, 1987 (c) Texas Instruments		   ;
;									   ;
;			       David Bartley				   ;
;									   ;
;		      Primitive Functions and Opcodes			   ;
;									   ;
;   tc	2/10/87 READ-STRING opcode added				   ;
;   rb	3/20/87 %XESC opcode added					   ;
;   rb	4/ 1/87 pcs-primop-+, -* modified; no error was being signalled    ;
;		for a single non-numeric argument to either + or * since   ;
;		pcs-primop-std-n2 assumes a unary arg is the operator's    ;
;		identity element and removes the operator; so, the	   ;
;		arg was never type-checked since the operator's handler    ;
;		never got called; now force unarys to binarys to keep	   ;
;		the operator						   ;
;   tc	4/13/87 make-string primop handler changed to handle optional	   ;
;		2nd argument						   ;
;									   ;
;--------------------------------------------------------------------------;


(define pcs-define-primop
  (lambda (op handler)
    (putprop op handler 'pcs*primop-handler)))


(define (pcs-primop-std-n2 form)  ; n-ary to binary, left associative
  (if (atom? form)
      `(%%get-global%% (quote ,form))			; funarg use
      (begin
	(pcs-chk-length>= form form 2)
	(cond ((null? (cddr form))			; unary?
	       (cadr form))				; --> identity
	      ((null? (cdddr form))
	       form)					; binary
	      (else
	       (let ((op   (car form))
		     (a    (cadr form))
		     (b    (caddr form))
		     (rest (cdddr form)))
		 (pcs-primop-std-n2
		  `(,op (,op ,a ,b) . ,rest))))))))


(define (pcs-primop-append* form)	; for append, append!, string-append
  (if (atom? form)
      `(%%get-global%% (quote ,form))			; funarg use
      (let ((op (car form)))
	(pcs-chk-length>= form form 1)
	(cond ((null? (cdr form))			; no args?
	       (if (eq? op 'STRING-APPEND)
		   ''""
		   ''()))
	      ((null? (cddr form))			; one arg?
	       (if (eq? op 'STRING-APPEND)
		   `(STRING-APPEND ,(cadr form) '"")
		   (cadr form)))
	      ((null? (cdddr form))			; two args?
	       (case op
		 ((APPEND) `(%APPEND . ,(cdr form)))
		 ((APPEND!) form)
		 (else
		  `(let ((%00000 ,(cadr form))
			 (%00001 ,(caddr form)))
		     (%STRING-APPEND %00000 0 (STRING-LENGTH %00000)
				     '()
				     %00001 0 (STRING-LENGTH %00001))))))
	      ((and (null? (cddddr form))
		    (eq? op 'STRING-APPEND))            ; 3 args
	       `(let ((%00000 ,(cadr form))
		      (%00001 ,(caddr form))
		      (%00002 ,(cadddr form)))
		  (%STRING-APPEND %00000 0 (STRING-LENGTH %00000)
				  %00001
				  %00002 0 (STRING-LENGTH %00002))))
	      (else
	       (let ((a (cadr form))
		     (b (caddr form))
		     (rest (cdddr form)))
		 (pcs-primop-append*
		  `(,op ,a (,op ,b . ,rest)))))))))


(define pcs-primop-+					; "+" handler
   (lambda (form)
     (if (and (not (atom? form))
	      (null? (cdr form)))
	 0
	 (if (and (not (atom? form)) 
                  (null? (cddr form)))
	     `(+ 0 ,(cadr form))
	     (pcs-primop-std-n2 form)))))


(define pcs-primop--					; "-" handler
   (lambda (form)
     (cond ((and (not (atom? form))
		 (not (atom? (cdr form)))
		 (null? (cddr form)))
	    `(minus ,(cadr form)))
	   (t (pcs-primop-std-n2 form)))))


(define pcs-primop-*					; "*" handler
   (lambda (form)
     (if (and (not (atom? form))
	      (null? (cdr form)))
	 1
	 (if (and (not (atom? form)) 
                  (null? (cddr form)))
	     `(* 1 ,(cadr form))
	     (pcs-primop-std-n2 form)))))


(define pcs-primop-/					; "/" handler
   (lambda (form)
     (cond ((and (not (atom? form))
		 (not (atom? (cdr form)))
		 (null? (cddr form)))
	    `(/ '1 ,(cadr form)))
	   (t (pcs-primop-std-n2 form)))))


(define (pcs-primop-vector form)			; "vector" handler
  (cond ((atom? form)
	 `(%%get-global%% (quote vector)))
	(else
	 `(list->vector (list . ,(cdr form))))))


(define (pcs-primop-list form)				; "list" handler
  (cond ((atom? form)
	 `(%%get-global%% (quote list)))
	((atom? (cdr form))	 ; (list)
	 ''())
	((atom? (cddr form))	 ; (list a)
	 form)
	((atom? (cdddr form))
	 (cons '%list2 (cdr form)))
	(else
	 (let ((rest (pcs-primop-list (cons 'list (cddr form)))))
	   `(cons ,(cadr form) ,rest)))))


(define (pcs-primop-list* form) 			; "list*" handler
  (cond ((atom? form)
	 `(%%get-global%% (quote list*)))
	((atom? (cdr form))	 ; (list*)
	 ''())
	((atom? (cddr form))	 ; (list* a)
	 (cadr form))
	(else
	 (let ((rest (pcs-primop-list* (cons 'list* (cddr form)))))
	   `(cons ,(cadr form) ,rest)))))


(define pcs-primop-make-vector				; "make-vector" handler
  (lambda (form)
    (cond ((atom? form)
	   `(%%get-global%% (quote ,form)))		; funarg use
	  ((and (not (atom? (cdr form)))		; unary?
		(null? (cddr form)))
	   form)
	  ((and (not (atom? (cdr form)))		; binary?
		(not (atom? (cddr form)))
		(null? (cdddr form)))
	   `(let ((%00000 (make-vector ,(cadr form))))
	      (begin (vector-fill! %00000 ,(caddr form))
		     %00000)))
	  (else
	   (pcs-chk-length= form form 3)))))


(define pcs-primop-io-1 				; optional PORT arg
  (lambda (form)
    (cond ((atom? form)
	   `(%%get-global%% (quote ,form)))		; funarg use
	  ((null? (cdr form))
	   `(,(car form) '()))                          ; add null port
	  ((and (not (atom? (cdr form)))
		(null? (cddr form)))
	   form)					; PORT supplied
	  (else
	   (pcs-chk-length= form form 2)))))

;
;  Note that make-string uses the following primop definition to take
;  care of its optional second argument.
;

(define pcs-primop-io-2 				; optional 2nd PORT arg
  (lambda (form)
    (cond ((atom? form)
	   `(%%get-global%% (quote ,form)))		; funarg use
	  ((and (not (atom? (cdr form)))
		(null? (cddr form)))			; add null port
	   `(,(car form) ,(cadr form) '()))
	  ((and (not (atom? (cdr form)))
		(not (atom? (cddr form)))
		(null? (cdddr form)))
	   form)					; PORT supplied
	  (else
	   (pcs-chk-length= form form 3)))))

;;; --------------------------------------------------------------------


;;;				!! NOTE !!

;;;  Each primitive operation defined with PCS-DEFINE-PRIMOP must also
;;;  be represented at runtime as a closure object in case the name is
;;;  used as a "funarg."  The error handler can auto-create such
;;;  closures when both PCS*PRIMOP-HANDLER and PCS*OPCODE properties are
;;;  integers.	Others must have such closures defined explicitly.  Many
;;;  of them are defined in the PCS source file PFUNARG.S.


;;; --------------------------------------------------------------------


(begin
 (pcs-define-primop  '%%bind-fluid%%     2)
 (pcs-define-primop  '%%car              1)
 (pcs-define-primop  '%%cdr              1)
 (pcs-define-primop  '%%def-global%%     2)
 (pcs-define-primop  '%%execute          1)
 (pcs-define-primop  '%%fasl             1)
 (pcs-define-primop  '%%fluid-bound?%%   1)
 (pcs-define-primop  '%%get-fluid%%      1)
 (pcs-define-primop  '%%get-global%%     1)
 (pcs-define-primop  '%%get-scoops%%     1)
 (pcs-define-primop  '%%set-fluid%%      2)
 (pcs-define-primop  '%%set-global%%     2)
 (pcs-define-primop  '%%set-scoops%%     2)
 (pcs-define-primop  '%%unbind-fluid%%   1)
 (pcs-define-primop  '%append            2)
 (pcs-define-primop  '%apply             2)
 (pcs-define-primop  '%begin-debug       0)
 (pcs-define-primop  '%call/cc           1)
 (pcs-define-primop  '%car               1)
 (pcs-define-primop  '%cdr               1)
 (pcs-define-primop  '%clear-registers   0)
 (pcs-define-primop  '%clear-window      1)
 (pcs-define-primop  '%close-port        1)
 (pcs-define-primop  '%compact-memory    0)
 (pcs-define-primop  '%define            3)
 (pcs-define-primop  '%env-lu            2)
 (pcs-define-primop  '%esc1              1)
 (pcs-define-primop  '%esc2              2)
 (pcs-define-primop  '%esc3              3)
 (pcs-define-primop  '%esc4              4)
 (pcs-define-primop  '%esc5              5)
 (pcs-define-primop  '%esc6              6)
 (pcs-define-primop  '%esc7              7)
 (pcs-define-primop  '%xesc (lambda (form) form))
 (pcs-define-primop  '%garbage-collect   0)
 (pcs-define-primop  '%graphics          7)
 (pcs-define-primop  '%halt              0)
 (pcs-define-primop  '%internal-time     0)
 (pcs-define-primop  '%list2             2)
 (pcs-define-primop  '%logxor            2)
 (pcs-define-primop  '%logand            2)
 (pcs-define-primop  '%logior            2)
 (pcs-define-primop  '%make-window       1)
 (pcs-define-primop  '%open-port         2)
 (pcs-define-primop  '%random            0)
 (pcs-define-primop  '%reify             2)
 (pcs-define-primop  '%reify!            3)
 (pcs-define-primop  '%reify-port        2)
 (pcs-define-primop  '%reify-port!       3)
 (pcs-define-primop  '%reify-stack       1)
 (pcs-define-primop  '%reify-stack!      2)
 (pcs-define-primop  '%restore-window    2)
 (pcs-define-primop  '%save-window       1)
 (pcs-define-primop  '%set-global-environment   1)
 (pcs-define-primop  '%sfpos             3)      ; set-file-position!
 (pcs-define-primop  '%start-timer       1)
 (pcs-define-primop  '%stop-timer        0)
 (pcs-define-primop  '%string-append     7)
 (pcs-define-primop  '%substring-display 5)
 (pcs-define-primop  '%transcript        1)
)

(begin
 (pcs-define-primop  '*          pcs-primop-*)
 (pcs-define-primop  '+          pcs-primop-+)
 (pcs-define-primop  '-          pcs-primop--)
 (pcs-define-primop  '/          pcs-primop-/ )
 (pcs-define-primop  '<          2)
 (pcs-define-primop  '<=         2)
 (pcs-define-primop  '<=?        2)
 (pcs-define-primop  '<>         2)
 (pcs-define-primop  '<>?        2)
 (pcs-define-primop  '<?         2)
 (pcs-define-primop  '=          2)
 (pcs-define-primop  '=?         2)
 (pcs-define-primop  '>          2)
 (pcs-define-primop  '>=         2)
 (pcs-define-primop  '>=?        2)
 (pcs-define-primop  '>?         2)
 (pcs-define-primop  'abs        1)
 (pcs-define-primop  'append     pcs-primop-append*)
 (pcs-define-primop  'append!    pcs-primop-append*)
 (pcs-define-primop  'assoc      2)
 (pcs-define-primop  'assq       2)
 (pcs-define-primop  'assv       2)
 (pcs-define-primop  'atom?      1)
 (pcs-define-primop  'caaar      1)
 (pcs-define-primop  'caadr      1)
 (pcs-define-primop  'caar       1)
 (pcs-define-primop  'cadar      1)
 (pcs-define-primop  'cadddr     1)
 (pcs-define-primop  'caddr      1)
 (pcs-define-primop  'cadr       1)
 (pcs-define-primop  'car        1)
 (pcs-define-primop  'cdaar      1)
 (pcs-define-primop  'cdadr      1)
 (pcs-define-primop  'cdar       1)
 (pcs-define-primop  'cddar      1)
 (pcs-define-primop  'cdddr      1)
 (pcs-define-primop  'cddr       1)
 (pcs-define-primop  'cdr        1)
 (pcs-define-primop  'ceiling    1)
 (pcs-define-primop  'char->integer 1)
 (pcs-define-primop  'char-ci<?     2)
 (pcs-define-primop  'char-ci=?     2)
 (pcs-define-primop  'char-downcase 1)
 (pcs-define-primop  'char-ready?   pcs-primop-io-1)
 (pcs-define-primop  'char-upcase   1)
 (pcs-define-primop  'char<?     2)
 (pcs-define-primop  'char=?     2)
 (pcs-define-primop  'char?      1)
 (pcs-define-primop  'closure?   1)
 (pcs-define-primop  'complex?   1)
 (pcs-define-primop  'cons       2)
 (pcs-define-primop  'continuation?  1)
 (pcs-define-primop  'display    pcs-primop-io-2)
 (pcs-define-primop  'environment-parent 1)
 (pcs-define-primop  'environment?       1)
 (pcs-define-primop  'eq?        2)
 (pcs-define-primop  'equal?     2)
 (pcs-define-primop  'eqv?       2)
 (pcs-define-primop  'even?      1)
 (pcs-define-primop  'float      1)
 (pcs-define-primop  'float?     1)
 (pcs-define-primop  'floor      1)
 (pcs-define-primop  'getprop    2)
 (pcs-define-primop  'integer->char  1)
 (pcs-define-primop  'integer?   1)
 (pcs-define-primop  'last-pair  1)
 (pcs-define-primop  'length     1)
 (pcs-define-primop  'list       pcs-primop-list)
 (pcs-define-primop  'list*      pcs-primop-list*)
 (pcs-define-primop  'list-tail  2)
 (pcs-define-primop  'make-packed-vector  3)
 (pcs-define-primop  'make-string pcs-primop-io-2) ; handle optional 2nd arg
 (pcs-define-primop  'make-vector         pcs-primop-make-vector)
 (pcs-define-primop  'max        pcs-primop-std-n2)
 (pcs-define-primop  'member     2)
 (pcs-define-primop  'memq       2)
 (pcs-define-primop  'memv       2)
 (pcs-define-primop  'min        pcs-primop-std-n2)
 (pcs-define-primop  'minus      1)
 (pcs-define-primop  'negative?  1)
 (pcs-define-primop  'newline    pcs-primop-io-1)
 (pcs-define-primop  'not        1)
 (pcs-define-primop  'number?    1)
 (pcs-define-primop  'object-hash        1)
 (pcs-define-primop  'object-unhash      1)
 (pcs-define-primop  'odd?       1)
 (pcs-define-primop  'pair?      1)
 (pcs-define-primop  'port?      1)
 (pcs-define-primop  'positive?  1)
 (pcs-define-primop  'prin1      pcs-primop-io-2)
 (pcs-define-primop  'princ      pcs-primop-io-2)
 (pcs-define-primop  'print      pcs-primop-io-2)
 (pcs-define-primop  'print-length  1)
 (pcs-define-primop  'proc?      1)
 (pcs-define-primop  'proplist   1)
 (pcs-define-primop  'putprop    3)
 (pcs-define-primop  'quotient   2)
 (pcs-define-primop  'rational?  1)
 (pcs-define-primop  'read-line  pcs-primop-io-1)
 (pcs-define-primop  'read-atom  pcs-primop-io-1)
 (pcs-define-primop  'read-char  pcs-primop-io-1)
 (pcs-define-primop  'real?      1)
 (pcs-define-primop  'remainder  2)
 (pcs-define-primop  'remprop    2)
 (pcs-define-primop  'reset      0)
 (pcs-define-primop  'reverse!   1)
 (pcs-define-primop  'round      1)
 (pcs-define-primop  'scheme-reset  0)
 (pcs-define-primop  'set-car!   2)
 (pcs-define-primop  'set-cdr!   2)
 (pcs-define-primop  'string->symbol     1)
 (pcs-define-primop  'string->uninterned-symbol  1)
 (pcs-define-primop  'string-append      pcs-primop-append*)
 (pcs-define-primop  'string-fill!       2)
 (pcs-define-primop  'string-length      1)
 (pcs-define-primop  'string-ref         2)
 (pcs-define-primop  'string-set!        3)
 (pcs-define-primop  'string?    1)
 (pcs-define-primop  'substring  3)
 (pcs-define-primop  'substring-find-next-char-in-set     4)
 (pcs-define-primop  'substring-find-previous-char-in-set 4)
 (pcs-define-primop  'symbol->string     1)
 (pcs-define-primop  'symbol?    1)
 (pcs-define-primop  'the-environment    0)
 (pcs-define-primop  '%make-hashed-environment 0)
 (pcs-define-primop  'truncate   1)
 (pcs-define-primop  'vector     pcs-primop-vector)
 (pcs-define-primop  'vector-fill!  2)
 (pcs-define-primop  'vector-length   1)
 (pcs-define-primop  'vector-ref         2)
 (pcs-define-primop  'vector-set! 3)
 (pcs-define-primop  'vector?    1)
 (pcs-define-primop  'window-save-contents     1)
 (pcs-define-primop  'window-restore-contents  2)
 (pcs-define-primop  'write      pcs-primop-io-2)
 (pcs-define-primop  'write-char pcs-primop-io-2)
 (pcs-define-primop  'zero?      1)
 )


;;; --------------------------------------------------------------------


(define pcs-define-opcode			;    !! NOTE !!
  (lambda (op opcode)				; negative values mark
    (putprop op opcode 'pcs*opcode)))           ; side-effecting operations

;;;		-- opcode assignments --

(begin
 (pcs-define-opcode  '%%car              064)    ; (%%car nil) => nil
 (pcs-define-opcode  '%%cdr              065)    ; (%%cdr nil) => nil
 (pcs-define-opcode  '%%fasl            -191)
 (pcs-define-opcode  '%*imm              084)
 (pcs-define-opcode  '%+imm              081)
 (pcs-define-opcode  '%/imm              086)
 (pcs-define-opcode  '%append            113)
 (pcs-define-opcode  '%apply            -056)
 (pcs-define-opcode  '%call/cc          -054)
 (pcs-define-opcode  '%car               089)    ; (%car nil) => #!unbound
 (pcs-define-opcode  '%cdr               090)    ; (%cdr nil) => #!unbound
 (pcs-define-opcode  '%clear-window     -211)
 (pcs-define-opcode  '%close-port       -177)
 (pcs-define-opcode  '%define           -220)
 (pcs-define-opcode  '%env-lu            219)
 (pcs-define-opcode  '%esc1             -232)
 (pcs-define-opcode  '%esc2             -233)
 (pcs-define-opcode  '%esc3             -234)
 (pcs-define-opcode  '%esc4             -235)
 (pcs-define-opcode  '%esc5             -236)
 (pcs-define-opcode  '%esc6             -237)
 (pcs-define-opcode  '%esc7             -238)
 (pcs-define-opcode  '%xesc             -239)
 (pcs-define-opcode  '%graphics         -215)
 (pcs-define-opcode  '%halt             -248)
 (pcs-define-opcode  '%list2             120)
 (pcs-define-opcode  '%logxor            125)
 (pcs-define-opcode  '%logand            126)
 (pcs-define-opcode  '%logior            127)
 (pcs-define-opcode  '%make-window      -208)
 (pcs-define-opcode  '%open-port        -176)
 (pcs-define-opcode  '%random           -091)
 (pcs-define-opcode  '%reify             216)
 (pcs-define-opcode  '%reify!           -226)
 (pcs-define-opcode  '%reify-port        210)
 (pcs-define-opcode  '%reify-port!      -209)
 (pcs-define-opcode  '%reify-stack       229)
 (pcs-define-opcode  '%reify-stack!     -230)
 (pcs-define-opcode  '%restore-window   -213)
 (pcs-define-opcode  '%save-window      -212)
 (pcs-define-opcode  '%set-global-environment -225)
 (pcs-define-opcode  '%sfpos            -231)     ; set-file-position!
 (pcs-define-opcode  '%start-timer      -174)
 (pcs-define-opcode  '%stop-timer       -175)
 (pcs-define-opcode  '%string-append     214)
 (pcs-define-opcode  '%substring-display  -172)
 (pcs-define-opcode  '%transcript       -189)
)
(begin
 (pcs-define-opcode  '*                  083)
 (pcs-define-opcode  '+                  080)
 (pcs-define-opcode  '-                  082)
 (pcs-define-opcode  '/                  085)
 (pcs-define-opcode  '<                  092)
 (pcs-define-opcode  '<=                 093)
 (pcs-define-opcode  '<=?                093)
 (pcs-define-opcode  '<>                 097)
 (pcs-define-opcode  '<>?                097)
 (pcs-define-opcode  '<?                 092)
 (pcs-define-opcode  '=                  094)
 (pcs-define-opcode  '=?                 094)
 (pcs-define-opcode  '>                  095)
 (pcs-define-opcode  '>=                 096)
 (pcs-define-opcode  '>=?                096)
 (pcs-define-opcode  '>?                 095)
 (pcs-define-opcode  'abs                149)
 (pcs-define-opcode  'append!           -112)
 (pcs-define-opcode  'assoc              110)
 (pcs-define-opcode  'assq               108)
 (pcs-define-opcode  'assv               109)
 (pcs-define-opcode  'atom?              128)
 (pcs-define-opcode  'caaar              070)
 (pcs-define-opcode  'caadr              071)
 (pcs-define-opcode  'caar               066)
 (pcs-define-opcode  'cadar              072)
 (pcs-define-opcode  'cadddr             078)
 (pcs-define-opcode  'caddr              073)
 (pcs-define-opcode  'cadr               067)
 (pcs-define-opcode  'car                064)    ; same as %%car
 (pcs-define-opcode  'cdaar              074)
 (pcs-define-opcode  'cdadr              075)
 (pcs-define-opcode  'cdar               068)
 (pcs-define-opcode  'cddar              076)
 (pcs-define-opcode  'cdddr              077)
 (pcs-define-opcode  'cddr               069)
 (pcs-define-opcode  'cdr                065)    ; same as %%cdr
 (pcs-define-opcode  'ceiling            153)
 (pcs-define-opcode  'char->integer      161)
 (pcs-define-opcode  'char-ci<?          195)
 (pcs-define-opcode  'char-ci=?          193)
 (pcs-define-opcode  'char-downcase      197)
 (pcs-define-opcode  'char-ready?        190)
 (pcs-define-opcode  'char-upcase        196)
 (pcs-define-opcode  'char<?             194)
 (pcs-define-opcode  'char=?             192)
 (pcs-define-opcode  'char?              156)
 (pcs-define-opcode  'closure?           129)
 (pcs-define-opcode  'complex?           137)   ; same as NUMBER?
 (pcs-define-opcode  'cons               079)
 (pcs-define-opcode  'continuation?      131)
 (pcs-define-opcode  'display           -179)
 (pcs-define-opcode  'environment-parent 218)
 (pcs-define-opcode  'environment?       157)
 (pcs-define-opcode  'eq?                100)
 (pcs-define-opcode  'equal?             102)
 (pcs-define-opcode  'eqv?               101)
 (pcs-define-opcode  'even?              132)
 (pcs-define-opcode  'float              150)
 (pcs-define-opcode  'float?             133)
 (pcs-define-opcode  'floor              152)
 (pcs-define-opcode  'getprop            116)
 (pcs-define-opcode  'integer->char      160)
 (pcs-define-opcode  'integer?           135)
 (pcs-define-opcode  'last-pair          166)
 (pcs-define-opcode  'length             165)
 (pcs-define-opcode  'list               111)
 (pcs-define-opcode  'list-tail		 122)
 (pcs-define-opcode  'make-packed-vector 171)
 (pcs-define-opcode  'make-string        201)
 (pcs-define-opcode  'make-vector        168)
 (pcs-define-opcode  'max                098)
 (pcs-define-opcode  'member             105)
 (pcs-define-opcode  'memq               103)
 (pcs-define-opcode  'memv               104)
 (pcs-define-opcode  'min                099)
 (pcs-define-opcode  'minus              151)
 (pcs-define-opcode  'negative?          147)
 (pcs-define-opcode  'newline           -181)
 (pcs-define-opcode  'not                136)
 (pcs-define-opcode  'number?            137)
 (pcs-define-opcode  'object-hash       -227)
 (pcs-define-opcode  'object-unhash      228)
 (pcs-define-opcode  'odd?               138)
 (pcs-define-opcode  'pair?              139)
 (pcs-define-opcode  'port?              140)
 (pcs-define-opcode  'positive?          148)
 (pcs-define-opcode  'prin1             -178)
 (pcs-define-opcode  'princ             -179)
 (pcs-define-opcode  'print             -180)
 (pcs-define-opcode  'print-length       184)
 (pcs-define-opcode  'proc?              141)
 (pcs-define-opcode  'proplist           118)
 (pcs-define-opcode  'putprop           -117)
 (pcs-define-opcode  'quotient           087)
 (pcs-define-opcode  'rational?          135)   ; same as INTEGER?
 (pcs-define-opcode  'read-line         -186)
 (pcs-define-opcode  'read-atom         -187)
 (pcs-define-opcode  'read-char         -188)
 (pcs-define-opcode  'real?              137)   ; same as NUMBER?
 (pcs-define-opcode  'remainder          088)
 (pcs-define-opcode  'remprop           -119)
 (pcs-define-opcode  'reset             -251)
 (pcs-define-opcode  'reverse!          -106)
 (pcs-define-opcode  'round              155)
 (pcs-define-opcode  'scheme-reset      -252)
 (pcs-define-opcode  'set-car!          -020)
 (pcs-define-opcode  'set-cdr!          -021)
 (pcs-define-opcode  'string->symbol     203)
 (pcs-define-opcode  'string->uninterned-symbol  204)
 (pcs-define-opcode  'string-fill!      -202)
 (pcs-define-opcode  'string-length      198)
 (pcs-define-opcode  'string-ref         199)
 (pcs-define-opcode  'string-set!       -200)
 (pcs-define-opcode  'string?            143)
 (pcs-define-opcode  'substring          167)
 (pcs-define-opcode  'substring-find-next-char-in-set     206)
 (pcs-define-opcode  'substring-find-previous-char-in-set 207)
 (pcs-define-opcode  'symbol->string     205)
 (pcs-define-opcode  'symbol?            144)
 (pcs-define-opcode  'the-environment    217)
 (pcs-define-opcode  '%make-hashed-environment 62)
 (pcs-define-opcode  'truncate           154)
 (pcs-define-opcode  'vector-fill!      -170)
 (pcs-define-opcode  'vector-length      169)
 (pcs-define-opcode  'vector-ref         011)
 (pcs-define-opcode  'vector-set!       -019)
 (pcs-define-opcode  'vector?            145)
 (pcs-define-opcode  'window-save-contents     -212)
 (pcs-define-opcode  'window-restore-contents  -213)
 (pcs-define-opcode  'write             -178)
 (pcs-define-opcode  'write-char        -179)
 (pcs-define-opcode  'zero?              146)
 )
;;; --------------------------------------------------------------------

(begin
 (pcs-define-opcode  'LOAD                000)
 (pcs-define-opcode  'LOAD-CONSTANT       001)
 (pcs-define-opcode  'LOAD-IMMEDIATE      002)
 (pcs-define-opcode  'LOAD-LOCAL          004)
 (pcs-define-opcode  'LOAD-LEX            005)
 (pcs-define-opcode  'LOAD-ENV            006)
 (pcs-define-opcode  'LOAD-GLOBAL         007)
 (pcs-define-opcode  'LOAD-FLUID          008)

 (pcs-define-opcode  'STORE-LOCAL        -012)
 (pcs-define-opcode  'STORE-LEX          -013)
 (pcs-define-opcode  'STORE-ENV          -014)
 (pcs-define-opcode  'STORE-GLOBAL       -015)
 (pcs-define-opcode  'STORE-GLOBAL-DEF   -031)
 (pcs-define-opcode  'STORE-FLUID        -016)

 (pcs-define-opcode  'POP                -024)
 (pcs-define-opcode  'PUSH               -025)
 (pcs-define-opcode  'DROP               -026)
 (pcs-define-opcode  'DROP-ENV           -061)
 (pcs-define-opcode  'PUSH-ENV           -221)
 (pcs-define-opcode  'BIND-FLUID         -029)
 (pcs-define-opcode  'UNBIND-FLUIDS      -030)
 (pcs-define-opcode  '%%fluid-bound?%%    134)

 (pcs-define-opcode  'J_S                -032)
 (pcs-define-opcode  'JN_S               -034)
 (pcs-define-opcode  'JNN_S              -036)
 (pcs-define-opcode  'JA_S               -038)
 (pcs-define-opcode  'JNA_S              -040)
 (pcs-define-opcode  'JE_S               -042)
 (pcs-define-opcode  'JNE_S              -044)

 (pcs-define-opcode  'J_L                -033)
 (pcs-define-opcode  'JN_L               -035)
 (pcs-define-opcode  'JNN_L              -037)
 (pcs-define-opcode  'JA_L               -039)
 (pcs-define-opcode  'JNA_L              -041)
 (pcs-define-opcode  'JE_L               -043)
 (pcs-define-opcode  'JNE_L              -045)

 (pcs-define-opcode  'CALL               -048)
 (pcs-define-opcode  'CALL-TR            -049)
 (pcs-define-opcode  'CCC                -050)
 (pcs-define-opcode  'CCC-TR             -051)
 (pcs-define-opcode  'CALL-CLOSURE       -052)
 (pcs-define-opcode  'CALL-CLOSURE-TR    -053)
 (pcs-define-opcode  'CCC-CLOSED         -054)
 (pcs-define-opcode  'CCC-CLOSED-TR      -055)
 (pcs-define-opcode  'APPLY-CLOSURE      -056)
 (pcs-define-opcode  'APPLY-CLOSURE-TR   -057)

 (pcs-define-opcode  'EXIT               -059)
 (pcs-define-opcode  'CLOSE              -060)

 (pcs-define-opcode  '%begin-debug       -255)
 (pcs-define-opcode  '%clear-registers   -253)
 (pcs-define-opcode  '%compact-memory    -247)
 (pcs-define-opcode  '%%execute          -058)
 (pcs-define-opcode  '%garbage-collect   -249)
 (pcs-define-opcode  '%internal-time      250)
 )
;;; --------------------------------------------------------------------

(begin
 (putprop '%begin-debug     #!true 'pcs*nilargop)    ; no source or dest
 (putprop '%clear-registers #!true 'pcs*nilargop)    ; no source or dest
 (putprop '%compact-memory  #!true 'pcs*nilargop)    ; no source or dest
 (putprop '%garbage-collect #!true 'pcs*nilargop)    ; no source or dest
 (putprop '%halt            #!true 'pcs*nilargop)    ; no source or dest
 (putprop 'reset            #!true 'pcs*nilargop)    ; no source or dest
 (putprop 'scheme-reset     #!true 'pcs*nilargop)    ; no source or dest
 )
;;; --------------------------------------------------------------------

(begin							; collect garbage
 (%clear-registers)
 (%compact-memory))

;;; --------------------------------------------------------------------
