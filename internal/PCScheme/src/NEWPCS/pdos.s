
;      -*- Mode: Lisp -*-			       Filename:  pdos.s

;--------------------------------------------------------------------------;
;									   ;
;			  TI SCHEME -- PCS Compiler			   ;
;		   Copyright 1985 (c) Texas Instruments 		   ;
;									   ;
;			       David Bartley				   ;
;									   ;
;			  DOS Interface Routines			   ;
;									   ;
;--------------------------------------------------------------------------;

;;; Revision history:
;;; ds	6/ 5/86 - added new file and directory functions
;;; rb	7/16/86 - DOS-CALL checks for .COM and .EXE files
;;; ds 12/08/86 - fixed a problem with dos-rename not correctly reseting the
;;;		  destination drive correctly.

;;;  The following Scheme function implements a directory listing
;;;  capability.  DOS-DIR is called with an MS-DOS filename specifier
;;;  which may contain wildcard characters, and returns a list of
;;;  the filenames which match the filespec.  For example,
;;;
;;;			(DOS-DIR "\\pcs\\*.exe")
;;;
;;;  might return the list:
;;;
;;;		       ("PCS.EXE" "MAKE_FSL.EXE")
;;;
;;; Remember that Scheme requires the backslash character "\" to be
;;; escaped, so you must specify two "\\"'s in a character string if
;;; you want to see one "\".

(begin

(define dos-dir
  (lambda (filespec)
    (letrec ((dir1 (lambda ()
		     (let ((next (%esc1 1)))
			  (if next
			      (cons next (dir1))
			      '())))))
      (if (string? filespec)
	  (let ((next (%esc2 0 filespec)))
	     (if next
		 (cons next (dir1))
		 '() ))
	  (%error-invalid-operand 'DOS-DIR filespec) ))))


;;;  The DOS-CALL function permits a user to issue any MS-DOS command from
;;;  Scheme and return when the function has completed.  The format for
;;;  the DOS-CALL function is:
;;;
;;;		(dos-call "filename" "parameters"
;;;			    {memory} {protect display})
;;;
;;;  where "filename" is the name of an .EXE or .COM file which is to
;;;			be executed.  If "filename" is a null (zero length)
;;;			string (i.e., ""), the "parameters" string is
;;;			passed to a new copy of COMMAND.COM.
;;;
;;;	   "parameters" is the parameter string to be passed to the
;;;			application or COMMAND.COM.
;;;
;;;			If both "filename" and "parameters" are null
;;;			strings, DOS-CALL exits to MS-DOS COMMAND.COM and
;;;			stays there until the command EXIT is entered, at
;;;			which time PCS execution resumes.
;;;
;;;	   "memory" is an optional argument which specifies the number
;;;			of paragraphs (16 byte units of memory) which are
;;;			to be freed up to run the requested task.  If this
;;;			argument is omitted, all available Scheme user
;;;			memory is made available to the task.  Note:
;;;			4096 paragraphs is equivalent to 64K bytes of
;;;			memory.
;;;
;;;	  "protect display" is an optional argument which allows the current
;;;			screen to be left undisturbed when the external program
;;;			is being executed.  Note: this will only inhibit text
;;;			from being displayed to the screen for programs doing
;;;			screen i/o that doesn't bypass the BIOS (Lotus 1-2-3
;;;			does).
;;;
;;;   Scheme memory is freed up by copying it to disk in 4095 paragraph
;;;   (65,520 byte) blocks.  Specifying 4095 paragraphs instead of 4096 (to
;;;   make it an even 64K bytes) saves a slight bit of disk I/O overhead.
;;;
;;;   The value returned by DOS-CALL is an integer error code.	Zero
;;;   indicates no error; -1 indicates an argument error; positive values
;;;   are those returned by DOS itself.


(define dos-call
  (lambda args
    (define extension-sans-filename
      ;given filename of form "file.ext" (leading directories are allowed)
      ;return extension ".ext" or empty string if none
      (lambda (file)
	(let ((period (substring-find-next-char-in-set
			file 0 (string-length file) ".")))
	  (if period
	      (substring file period (string-length file))
	      ""))))
    (let ((filename (if args (car args) ""))
	  (parameters (if (and args (cadr args)) (cadr args) ""))
	  (mem_req (if (cddr args) (car (cddr args)) 0))
	  (protect (if (= (length (cddr args)) 2) (cadr (cddr args)) 0))
	  (temp-window (%make-window '()))
          (window-contents '()))
     ;body of DOS-CALL
     (if (and (string? filename)
	      (string? parameters)
	      (cond ((string-null? filename))   ;null name means just go to DOS
		    ((string-ci=? (extension-sans-filename filename) ".COM"))
		    ((string-ci=? (extension-sans-filename filename) ".EXE"))
		    (t nil)))		;any other extension illegal
       (begin
	(if (eqv? protect 0)
	  (begin
	    (set! window-contents (%save-window temp-window))
	    (%clear-window temp-window)))
	(begin0
	  (%esc5
	     2
	     filename
	     (if (eqv? filename "")
		 (if (eqv? parameters "")
		     (list->string (list (integer->char 0)
					 (integer->char 13)))
		     (string-set!
			 (string-append
			     (string-append "x/c " parameters)
			     (make-string 1 #\return))
			 0
			 (integer->char (+ (string-length parameters) 3))))
		 (string-set!
		    (string-append
		       (string-append "x" parameters)
		       (make-string 1 #\return))
		    0
		    (integer->char (string-length parameters))))
	     (truncate mem_req)
	     protect)

	 (if (eqv? protect 0)
	    (begin
	      (let ((cur_pos (window-get-cursor 'console)))
		   (%clear-window 'console)
		   (window-set-cursor! 'console (car cur_pos) (cdr cur_pos))
		   (%restore-window temp-window window-contents))))
	  ))
      -1))))				; error


;;;  The following Scheme function implements a software interrupt
;;;  capability.  SW-INT is called with an interrupt number between
;;;  0 and 255, a return result value, and up to four values which
;;;  will be stuffed into the registers ax,bc,cx,and dx.
;;;
;;;  Possible values for the return result are:
;;;		0 - INTEGER
;;;		1 - T OR NIL
;;;		2 - STRING
;;;
;;;  (SW-INT 112 0 100 "hello") -
;;;	Invokes interrupt 112 (or 70 hex). Register ax will be loaded
;;;	with a pointer to 100, bx will be loaded with a pointer to
;;;	the string "hello" and registers cx and dx are not used. The
;;;	return value is expected to be an integer. (return values are
;;;	handled the same way that Lattice C expects results from assembly
;;;	language programs.)
;;;

(define sw-int
  (lambda args
     (let ((int_num (car args))
	   (return_type (cadr args))
	   (ax (if (null? (cddr args)) "" (caddr args)))
	   (bx (if (null? (cdddr args)) "" (cadddr args)))
	   (cx (if (null? (cddddr args)) "" (car (cddddr args))))
	   (dx (if (null? (cdr(cddddr args))) "" (cadr(cddddr args)))))
	  (if (> (length args) 6)
	     (apply %error-invalid-operand-list (cons 'SW-INT args))
	  ;else
	     (if (or (< int_num 0) (> int_num 255))
	       (%error-invalid-operand 'SW-INT int_num)
	     ;else
	       (if (> return_type 3)
		 (%error-invalid-operand 'SW-INT return_type)
	       ;else
		 (%esc7 7 int_num return_type ax bx cx dx)))))))

;;;
;;;  The following Scheme function implements a file deletion
;;;  capability.  DOS-DELETE is called with an MS-DOS filename
;;;  specifier which may NOT contain wildcard characters. The file
;;;  specification can conatin drive and path specifications. An
;;;  integer is returned indicating if the result was successful or not.
;;;  A successful call will return 0, anything else indicates an error.
;;;  For example:
;;;
;;;			(DOS-DELETE "temp.exe")
;;;

(define dos-delete
  (lambda (filespec)
     (if (string? filespec)
	 (if (file-exists? filespec)
	     (%esc2 10 filespec)
	     (error "DOS-DELETE: File does not exist!"))
	 (error "DOS-DELETE: Must specify a string!"))))

;;;
;;;  The following Scheme function implements a capability to copy
;;;  DOS files. DOS-FILE-COPY is called with two MS-DOS filename
;;;  specifiers. The first file must exist in the current directory,
;;;  the second will be over written over if it does exist or created
;;;  if it doesn't. The file specifications may NOT contain wildcard
;;;  characters. The source file can contain a path specification.
;;;  A drive designator may be specified as the destination
;;;  but the destination may not be blank. If just a drive designation
;;;  is entered then the source file name is appended to the destination.
;;;  An integer is returned indicating if the call was successful or not.
;;;  A zero indicates a successfull call, anything else indicates an error.
;;;  For example:
;;;
;;;			(DOS-FILE-COPY "temp.exe" "temp.xxx")
;;;
;;; Remember that Scheme requires the backslash character "\" to be
;;; escaped, so you must specify two "\\"'s in a character string if
;;; you want to see one "\".

;;;  compare-spec will return a number that is the first occurence of
;;;  either a backslash or a colon that is not part of the file name.

(define compare-spec
  (lambda (len filespec)
    (if (and (>? len 0)
	     (not (char-ci=? (string-ref filespec (-1+ len)) #\\))
	     (not (char-ci=? (string-ref filespec (-1+ len)) #\:)))
	(compare-spec (-1+ len) filespec)
	len)))

;;;  strip-path will take a filespec as input and return just the file
;;;  name without the path specification.

(define strip-path
  (lambda (filespec)
    (substring filespec (compare-spec (string-length filespec) filespec)
	       (string-length filespec))))

(define dos-file-copy
  (lambda (filespec1 filespec2)
     (if (and (string? filespec1) (string? filespec2))
	 (if (file-exists? filespec1)
	     (begin

; if filespec2 is two characters where the second character is a colon
; and the first is a letter between A and J then append the filespec1

	     (if (and (equal? (string-length  filespec2) 2)
		      (equal? (string-ref filespec2 1) #\:)
		      (char-ci>=? (string-ref filespec2 0) #\a)
		      (char-ci<=? (string-ref filespec2 0) #\j))

; now if filespec1 contains a pathname then only append the file name
; portion

		 (set! filespec2 (string-append filespec2
						(strip-path filespec1))))

	     (%esc3 11 filespec1 filespec2))
	     (error "DOS-FILE-COPY: File does not exist!"))
	 (error "DOS-FILE-COPY: Must specify a string!"))))

;;;
;;;  The following Scheme function implements a capability to rename
;;;  files in the current directory. DOS-RENAME is called with two
;;;  MS-DOS filename specifiers. The first must exist and the second
;;;  cannot exist. The filename specifiers may NOT contain wildcard
;;;  characters. The first file name can include drive and path
;;;  specifications, the second cannot. An integer is returned
;;;  indicating if the call was successful or not. For example:
;;;
;;;			(DOS-RENAME "temp.exe" "temp.xxx")
;;;
;;; Remember that Scheme requires the backslash character "\" to be
;;; escaped, so you must specify two "\\"'s in a character string if
;;; you want to see one "\".

;;;  get-dir will change directories and if neccessary drives and
;;;  return the previous path specification.

(define get-dir
  (lambda (filespec p-len)
    (let ((old-drive '())
	  (old-dir '())
	  (path-spec (substring filespec 0 p-len )))

;;;  p-len will be zero if there is no path or drive specification
;;;  first use dos-chdir to change directories and then if necessary
;;;  change drives
      (when (<>? p-len 0)
	    (set! old-drive (substring (dos-chdir " ") 0 2))
	    (if (and (>? p-len 1)
		     (equal? (string-ref path-spec 1) #\:))
		(dos-change-drive (substring path-spec 0 2)))
	    (if (and (>? p-len 1)
		     (equal? (string-ref path-spec (-1+ p-len)) #\\)
		     (not (equal? (string-ref path-spec (- p-len 2))
				  #\:)))
		(string-set! path-spec (-1+ p-len) #\ ))
	    (set! old-dir (dos-chdir path-spec)))
      (list old-dir old-drive))))

;;;  reset-dir will change back to the original drive and path
;;;  specification, if necessary.

(define reset-dir
  (lambda (old-specs)
    (when (not (equal? old-specs '(() ()) ))
	  (dos-chdir (car old-specs))
	  (dos-change-drive (cadr old-specs))
	  )))

(define dos-rename
  (lambda (filespec1 filespec2)

    (if (and (string? filespec1) (string? filespec2))
	(if (file-exists? filespec1)
	    (let ((path-spec (get-dir filespec1
				      (compare-spec (string-length filespec1)
						    filespec1)))
		  (return 0))
      ; if there is a drive  or path to change to that has been done.
      ; now check if the destination file exists
	      (if (not (file-exists? filespec2))
		  (set! return (%esc3 12 (strip-path filespec1) filespec2))
		  (error "DOS-RENAME: Destination file exists!"))
	      (reset-dir path-spec)
	      return)
	    (error "DOS-RENAME: Source file does not exist!"))
	(error "DOS-RENAME: Must specify a string!"))))

;;;
;;;  The following Scheme function implements a file size capability
;;;  DOS-FILE-SIZE is called with an MS-DOS filename specifier
;;;  which may NOT contain wildcard characters, and returns
;;;  an integer indicating the size of the file. For example:
;;;
;;;			(DOS-FILE-SIZE "temp.exe")
;;;

(define dos-file-size
  (lambda (filespec)
     (if (string? filespec)
	 (if (file-exists? filespec)
	     (%esc2 15 filespec)
	     (error "DOS-FILE-SIZE: File does not exist!"))
	 (error "DOS-FILE-SIZE: Must specify a string!"))))

;;;
;;;  The following Scheme function implements a capability to change
;;;  the current directory. DOS-CHDIR is called with a string
;;;  containing the directory which will become the current directory.
;;;  A string is returned which contains the previous directory.
;;;  For example:
;;;
;;;			(DOS-CHDIR "a:\\source")
;;;
;;; Remember that Scheme requires the backslash character "\" to be
;;; escaped, so you must specify two "\\"'s in a character string if
;;; you want to see one "\".
;;;

(define dos-chdir
  (lambda directory
     (if (null? directory)
       (%esc2 16 "")
     ;else
       (if (string? (car directory))
	 (%esc2 16 (car directory))
	 (error "DOS-CHDIR: Argument must be a string!")))))
;
; I personally like the following better, but above will ship for
; compatibility sake.
;
;(define dos-chdir
;  (lambda dir
;     (if (not (null? dir))
;       (if (string? (car dir))
;	 (let* ((old-dir (%esc2 16 (car dir)))	; change directory
;		(new-dir (%esc2 16 "")))	; get new directory
;	   (if (string-ci=? old-dir new-dir)	; if new = old?
;	     '()				;   return failure
;	     old-dir))				; else return old dir
;     	 (error "DOS-CHDIR: Argument must be a string"))
;    ;else
;      (%esc2 16 ""))))

;;;
;;;  The following Scheme function implements a capability to change
;;;  the current drive. DOS-CHANGE-DRIVE is called with a string
;;;  containing the drive which is to become the current drive.
;;;  #!TRUE is returned if the call was successful or not.
;;;  For example:
;;;
;;;			(DOS-CHANGE-DRIVE "a:")
;;;

(define dos-change-drive
  (lambda (filespec)
     (if (string? filespec)
	 (%esc2 17 filespec)
	 (error "DOS-CHANGE-DRIVE: Must specify a string!"))))

)
