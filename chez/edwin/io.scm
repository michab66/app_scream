;;;
;;;	Copyright (c) 1985 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;     Modified by Texas Instruments Inc 8/15/85
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; File IO

(define read-buffer
  (lambda (buffer filename)
    (region-delete! (buffer-region buffer))
    (if (file-exists? filename)
        (begin
          (let ((region (file->region-interactive filename)))
            (vector-set! (current-window) window:point
                         (mark-right-inserting (buffer-start buffer)))
            (region-insert! (buffer-start buffer) region))
	  (set-current-point! (buffer-start buffer)))
        (temporary-message "(New File)"))
    (set-buffer-truename! buffer filename)
    (set-buffer-pathname! buffer filename)
    (buffer-not-modified! buffer)))

(define insert-file
  (lambda (mark filename)
    (if (file-exists? filename)
        (region-insert! mark (file->region-interactive filename))
        (editor-error (string-append "File " filename " not found")))))

(define file->region-interactive
  (lambda (filename)
    (temporary-message (string-append "Reading file " filename))
    (let ((region (file->region filename)))
      (append-message " -- done")
      region)))

(define file->region
  (lambda (filename)
    (let ((port '()))
      (dynamic-wind
        (lambda () (set! port (open-input-file filename)))
        (lambda () (file-stream->region port))
        (lambda () (close-input-port port))))))

(define (file-stream->region stream)
  (let ((first-line (read-line stream)))
    (if (not (eof-object? first-line))
	(let ((first-line (make-line first-line))
	      (group (make-group #!FALSE)))
          (define (%connect-lines previous-line this-line n)
	    (connect-lines! previous-line this-line)
	    (set-line-group! this-line group)
	    (set-line-number! this-line n))
	  (define (loop previous-line n this-line)
            (if (not (eof-object? this-line))
		(let ((this-line (make-line this-line)))
                  (%connect-lines previous-line this-line n)
		  (loop this-line (+ n line-number-increment)
			(read-line stream)))
		(let ((this-line (make-line "")))
                  (%connect-lines previous-line this-line n)
		  (let ((region
			  (components->region first-line 0 this-line
				 	   (line-length this-line))))
		    (%set-group-region! group region)
		    region))))
	  (set-line-group! first-line group)
	  (set-line-number! first-line 0)
	  (loop first-line line-number-increment (read-line stream)))
	(let ((line (make-line "")))
          (lines->region line line)))))


;;;; Output
(define write-buffer
  (lambda (buffer filename)
    (if (or (not (file-exists? filename))
            (prompt-for-confirmation?
	       (string-append "File " filename
                              " exists.  Write anyway (Y or N)?")))
        (begin
          (temporary-message (string-append "Writing file " filename))
          (region->file (buffer-region buffer) filename)
          (append-message " -- done")
          (set-buffer-pathname! buffer filename)
          (set-buffer-truename! buffer filename)
          (buffer-not-modified! buffer)))))

(define write-region
  (lambda (region filename)
    (if (or (not (file-exists? filename))
            (prompt-for-confirmation?
	       (string-append "File " filename
                              " exists.  Write anyway (Y or N)?")))
        (begin
          (temporary-message (string-append "Writing file " filename))
          (region->file region filename)
          (append-message " -- done")))))

(define (region->file region filename)
  (let ((port '()))
    (dynamic-wind
      (lambda () (set! port (open-output-file filename)))
      (lambda () (region->filestream region port))
      (lambda () (close-output-port port)))))

(define (region->filestream region stream)
  (region-components region
    (lambda (start-line start-position end-line end-position)
      (if (eq? start-line end-line)
	  (princ (substring (line-string start-line)
				      start-position
				      end-position)
                 stream)
	  (begin
	   (princ (substring (line-string start-line)
				       start-position
				       (line-length start-line))
                  stream)
	   (let loop ((this-line (line-next start-line)))
	     (princ #\newline stream)
	     (if (eq? this-line end-line)
                 (princ (substring (line-string end-line)
                                   0
                                   end-position)
                        stream)
		 (begin (princ (line-string this-line) stream)
                        (loop (line-next this-line))))))))))

(define (save-buffer-changes buffer)
  (if (and (buffer-pathname buffer)
	   (buffer-modified? buffer)
	   (buffer-writeable? buffer)
	   (prompt-for-confirmation?
	    (string-append "Buffer "
                           (buffer-name buffer)
                           " contains changes.  Write them out (Y or N)?")))
      (write-buffer buffer (buffer-pathname buffer))))

(define (%save-buffer-changes buffer)
  (if (and (buffer-modified? buffer)
	   (buffer-writeable? buffer)
	   (prompt-for-confirmation?
	    (string-append "Buffer "
                           (buffer-name buffer)
                           " contains changes.  Write them out (Y or N)?")))
       (save-file buffer)))

(define (setup-current-buffer-read-only! argument)
  ((cond ((or (not argument) (zero? argument)) set-buffer-writeable!)
	 ((negative? argument) set-buffer-read-only!)
	 (else set-buffer-file-read-only!))
   (current-buffer)))

(define (save-file buffer)
  (if (buffer-modified? buffer)
      (if (or (buffer-writeable? buffer)
	      (prompt-for-confirmation?
	       (string-append "Buffer " (buffer-name buffer)
                               " is read only.  Save anyway (Y or N)?")))
	  (write-buffer buffer
			(let ((pathname (buffer-pathname buffer)))
                          (if (not pathname)
                              (prompt-for-pathname
                                "Write buffer to file : ")
                              pathname))))
      (temporary-message "(No changes need to be written)")))


