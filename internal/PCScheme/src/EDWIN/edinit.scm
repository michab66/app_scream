;;;
;;; Export EDWIN and EDWIN-RESET-WINDOWS to user-global-environment
;;;

(set! (access edwin user-global-environment) edwin)
(set! (access edwin-reset-windows user-global-environment)
      (access edwin-reset-windows edwin-environment))

;;;
;;; Export function which can remove edwin and reclaim all space
;;;

(set! (access remove-edwin user-global-environment)
  (lambda ()
    ;;;
    ;;; Remove autoload info from proplist)
    ;;;
    (remove-autoload-info "EDWIN0.FSL")
    (remove-autoload-info "edwin1.fsl")
    (remove-autoload-info "edwin2.fsl")
    (remove-autoload-info "edwin3.fsl")
    (remove-autoload-info "edwin4.fsl")
    (remove-autoload-info "edwin5.fsl")
    (remove-autoload-info "edwin6.fsl")
    (remove-autoload-info "edwin7.fsl")
    (remove-autoload-info "edwin8.fsl")
    (remove-autoload-info "edwin9.fsl")
    (remove-autoload-info "edwin10.fsl")
    ;;;
    ;;; Remove macros so there are no ties to EDWIN-ENVIRONMENT
    ;;;
    (remprop 'make-new-state 'PCS*MACRO)
    (remprop '%in-out-flag 'PCS*MACRO)
    (remprop '%before 'PCS*MACRO)
    (remprop '%after 'PCS*MACRO)
    (remprop '%next 'PCS*MACRO)
    (remprop '%set-next 'PCS*MACRO)
    (remprop 'define-initial-command-key 'PCS*MACRO)
    (remprop 'string-allocate 'PCS*MACRO)
    (remprop 'remap-edwin-key 'PCS*MACRO)
    ;;;
    ;;; Unbind REMOVE-EDWIN and EDWIN-ENVIRONMENT
    ;;; 
    (unbind 'edwin-reset-windows user-global-environment)
    (unbind 'edwin-environment user-global-environment)
    (unbind 'remove-edwin user-global-environment)
    ;;;
    ;;; Set EXIT and EDWIN definitions back to original
    ;;;
    (set! (access exit user-global-environment)
	  (access system-exit user-global-environment))

    (set! (access edwin user-global-environment)
	  (access initiate-edwin user-global-environment))
  ))

(set! (access system-exit user-global-environment) 
      (access exit user-global-environment))

(define %edwin-buffer%
  (lambda ()
       (vector-ref (vector-ref edwin-editor 1) 7)))

(define edwin-buffer-modified?
  (lambda (buf)
    (vector-ref buf 5)))

(define exit
  (lambda ()
    (cond ((or (unbound? edwin-editor)
	       (unassigned? edwin-editor))
           (system-exit))
          (else
           (%save-buffer-changes (%edwin-buffer%))
           (if (edwin-buffer-modified? (%edwin-buffer%))
             (if (prompt-for-confirmation? "Exit anyway (Y or N)?")
               (system-exit))
             (system-exit))
           (gc)))))

(set! (access exit user-global-environment) exit)
 
(macro remap-edwin-key
  (lambda (e)
     `(set-edwin-key  ,(cadr e) (comtab-entry ,(caddr e)))))

(define set-edwin-key
  (letrec
    ((%prefix
       (lambda (alists char)
         (%set-comtab-entry! alists char %command)))
     (%command '()))

    (lambda (char command)
      (cond ((char? char)
             (%set-comtab-key comtab (char-upcase char) command)
             (if (char-alphabetic?  char)
                 (%set-comtab-key comtab (char-downcase char) command)))
            ((and (pair? char) (null? (cdr char)))
             (%set-comtab-key comtab (char-upcase (car char)) command)
             (if (char-alphabetic?  (car char))
                 (%set-comtab-key comtab (char-downcase (car char)) command)))
            ((pair? char)
             (set! %command command)
             (comtab-lookup-prefix char %prefix))
          ((char-set? char)
           (mapc (lambda (char) (set-comtab-entry! char command))
                 (char-set-members char)))
          (else (error "Unknown character" char)))
      char)))

(let* ((last-char (string-ref pcs-sysdir (-1+ (string-length pcs-sysdir))))
       (edwin-init (%system-file-name "EDWIN.INI")))
  (if (file-exists? edwin-init)
      (load edwin-init)))
