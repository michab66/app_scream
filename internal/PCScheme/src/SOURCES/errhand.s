;
; The following code is an example of an error handler for I/O errors. The
; function open-input-file attempts to open filename for input. Note that 
; a continuation is saved in the fluid variable my%ioerr before the call to 
; open-input-file. Upon return from the open, the variable port is 
; interrogated to determine the status- To retry the operation with the same 
; filename, retry the operation with a different filename, or return the port
; object. 
;

(define (open-input-file filename)
  (let ((port (call/cc
                (fluid-lambda (my%ioerr) 
		   ((access open-input-file user-global-environment)
		    filename)))))
    (cond ((eq? port 'retry)
           (open-input-file filename))
          ((string? port)
           (open-input-file port))
          (else
            port))))
 
;          
; *USER-ERROR-HANDLER* has been designed to trap on all I/O errors, pop up a 
; window to indicate the error, and illicit a response from the user. The 
; result is then returned via the continuation bound to the fluid variable 
; my%ioerr. The system error handler is called for all other errors.
;
; See the User's Guide for a discussion on user error handling and a list of 
; all I/O errors.
;

(set! (access *user-error-handler* user-global-environment)
      (lambda (error-num error-msg irritant sys-error-handler)
        (if (and (fluid-bound? my%ioerr)
		 (number? error-num)
                 (>= error-num 1)
                 (<= error-num 88))
            (let ((win (make-window error-msg #t))
                  (result '()))
              (window-set-position! win 10 10)
              (window-set-size! win 6 50)
              (window-set-cursor! win 2 5)
              (window-popup win)
              (case error-num
                ((2 3)                           ;file/path not found
                 (display "File/Path not found : " win)
                 (display irritant win)
                 (newline win)
                 (display "Enter new pathname (or return to exit) - " win)
                 (set! result (read-line win))
                 (if (string=? result "")
                     (set! result '())))
                ((21)                           ;drive not ready
                 (display "Drive not ready - Retry (y/n)?" win)
                 (set! result 
                       (if (char=? (char-upcase (read-char win)) #\Y)
                           'retry
                           '())))
                (else
                  (display "Extended Dos I/O Error - " win)
                  (display irritant win) 
                  (newline win)
                  (newline win)
                  (char-upcase (read-char win))
                  (set! result '())))

              (window-popup-delete win)
	      ((fluid my%ioerr) result))
	;else
            (sys-error-handler))))

