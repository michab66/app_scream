;;;;          APPENDIX: HELP SYSTEM SOURCE CODE

;;;;
;;;;    A Help facility for PC Scheme
;;;;
;;;;    Precis of instructions:
;;;;      1. Load this file, i.e., type (load "help.s")
;;;;      2. To extract information on the definitions
;;;;         in a file of Scheme source code, type
;;;;         (extract-help "filename").
;;;;      3. To extract the help information and
;;;;         at the same time load the file, type
;;;;         (load-with-help "filename").
;;;;      4. Type (help 'ident) for information on the
;;;;         name ident.
;;;;      5. Type (help), without arguments, for a list
;;;;         of all identifiers for which extended
;;;;         help is available.

(define help
  (lambda subject
    (if (null? subject)
        (show-help-topics)
        (fetch-help (car subject)))
    *the-non-printing-object*))


(define fetch-help
  (lambda (item)
    (report-help item
                 (get-internal-help item)
                 (get-archival-help item))))

(define get-internal-help
  (lambda (item)
    (let ((item-class (classify item)))
      (if (and (symbol? item) (bound? item))
          (let* ((value       (eval item))
                 (value-class (classify value)))
            (list item-class value value-class))
          (list item-class)))))


(define classify
  (lambda (x)
    (cond ((pair?        x) 'pair)
          ((procedure?   x) (cond ((closure?      x) 'procedure)
                                  ((continuation? x) 'continuation)
                                  (else              'engine)))
          ((boolean?     x) 'boolean)
          ((symbol?      x) 'symbol)
          ((environment? x) 'environment)
          ((stream?      x) 'stream)
          ((port?        x) 'port)
          ((number?      x) 'number)
          ((char?        x) 'character)
          ((string?      x) 'string)
          ((vector?      x) 'vector)
          (else             'unknown))))


(define bound?
  (lambda (ident)
    (not (eval `(unbound? ,ident)))))


(define archive
  (let ((a-list '() ))
    (lambda (msg . args)
      (case msg
        ((get)     (cadr (assq (car args) a-list)))
        ((put)     (archive 'remove (car args))
                   (set! a-list (cons args a-list)))
        ((keys)    (map car a-list))
        ((remove)  (set! a-list (delq! (assq (car args) a-list) a-list)))
        (else      (error "Unrecognized message to archive:" msg))))))


(define get-archival-help
  (lambda (item)
    (archive 'get item)))


(define show-help-topics
  (lambda ()
    (writeln "Topics for which extended help is available:")
    (for-each writeln (archive 'keys))))


(define extract-help
  (lambda (filename)
    (with-input-from-file filename
      (lambda ()
        (do ((next (read) (read)))
            ((eof-object? next) 'OK)
            (let ((info (parse next)))
              (when info (put-archival-help filename info))))))))


(define parse
  (lambda (expr)
    (if (and (pair? expr) (eq? (car expr) 'define))
        (if (pair? (cadr expr))
            (parse-mit (cadr expr))
            (parse-iu (cdr expr)))
        '() )))


(define parse-mit
  (lambda (expr)
    (if (pair? (car expr))
        (parse-mit (car expr))
        (parse-params (car expr) (cdr expr)))))


(define parse-iu
  (lambda (expr)
    (let ((lambda-form (get-lambda (cadr expr))))
      (if lambda-form
          (parse-params (car expr) (cadr lambda-form))
          '() ))))


(define get-lambda
  (lambda (e)
    (if (or (null? e) (atom? e))
        '()
        (case (car e)
          ((lambda) e)
          ((let let* letrec) (get-lambda (car (last-pair e))))
          (else '() )))))


(define parse-params
  (lambda (name paramlist)
    (let loop ((params paramlist) (count 0))
      (cond ((null? params) (list name count 0 paramlist))
            ((atom? params) (list name count 1 paramlist))
            (else (loop (cdr params) (+ 1 count)))))))


(define put-archival-help
  (lambda (filename info)
    (archive 'put (car info) (append (list filename)
                                     (cdr info)))))


(define load-with-help
  (lambda (filename)
    (extract-help filename)
    (load filename)))


(define report-help
  (lambda (item internal-info archival-info)
    (let ((item-class  (car   internal-info))
          (value       (cadr  internal-info))
          (value-class (caddr internal-info)))
      (newline)
      (cond ((not (symbol? item)) (report-literal item item-class))
            ((null? value-class)  (report-unbound item))
            (else                 (report-binding item value value-class)))
      (when archival-info (report-archival item archival-info)))))


(define report-literal
  (lambda (item class)
    (writeln item " is an object of type " class ".")
    (newline)))


(define report-unbound
  (lambda (item)
    (writeln "The identifier " item " is unbound.")
    (newline)))


(define report-binding
  (lambda (item value class)
    (writeln "The identifier " item
             " is bound to an object of type " class ".")
    (when (denotable? class)
          (writeln "The value of " item " is " value "."))
    (newline)))
(define denotable?
  (lambda (class)
    (memq class '(boolean number character string vector pair symbol))))


(define report-archival
  (lambda (item info)
    (let* ((filename (car    info))
           (req-args (cadr   info))
           (opt-args (caddr  info))
           (params   (cadddr info))
           (argstr   (if (= 1 req-args) "argument" "arguments"))
           (optstr   (if (zero? opt-args) "no" "any number of")))
      (writeln item " is defined in file " filename)
      (writeln "as a procedure of " req-args " required " argstr)
      (writeln "and " optstr " optional arguments.")
      (writeln "The parameters to " item " are declared as follows:")
      (writeln params)
      (newline))))


