;****************************************************************************
;* SET-FILE-POSITION will move the file pointer to a new position           *
;* and update a pointer in the buffer to point to a new location.           *
;* The offset variable can be:                                              *
;*                     0 for positioning from the start of the file         *
;*                     1 for positioning relative to the current position   *
;*                     2 for positioning from the end of the file           *
;****************************************************************************

(define set-file-position!                 ; ==> filepos.s
  (lambda (port #-of-bytes offset)
    (let ((current-pos (%reify-port port 9))
          (end-of-buffer (%reify-port port 10))
          (new-pos '())
          (current-chunk (max 0 (-1+ (%reify-port port 12))))
          (new-chunk '())
          (messages '())
          (file-size (+ (* (%reify-port port 4) 65536) (%reify-port port 6)))
          (port-flags (%reify-port port 11)))
      (if (and (port? port)
               (=? (%logand port-flags 4) 0))
          (case offset
            ((0) ; offset from the start of the file
             (set! #-of-bytes (abs #-of-bytes))
             (if (=? (%logand port-flags 3) 0)
                 (set! #-of-bytes (min #-of-bytes file-size)))
             (set! new-chunk (truncate (/ #-of-bytes 256)))
             (set! new-pos (- #-of-bytes (* new-chunk 256)))
             (if (and (<? new-pos end-of-buffer)
                      (>=? new-pos 0)
                      (=? (%logand port-flags 3) 0) ; open for reading
                      (=? new-chunk current-chunk))
                 (%reify-port! port 9 new-pos)
                 (%sfpos port new-chunk new-pos)))

            ((1) ; offset from the current position
             (set! new-pos (+ current-pos #-of-bytes))
             (if (and (<? new-pos end-of-buffer)
                      (>=? new-pos 0)
                      (=? (%logand port-flags 3) 0)) ; open for reading
                 (%reify-port! port 9 new-pos)
                 (begin
                   (set! new-pos (+ (+ current-pos (* 256 current-chunk))
                                    #-of-bytes)) ; offset from the begining of the file
                   (if (and (>? new-pos file-size)
                            (=? (%logand port-flags 3) 0))
                       (set! new-pos file-size))
                   (if (<? new-pos 0)
                       (set! new-pos 0))
                   (set! new-chunk (truncate (/ new-pos 256)))
                   (%sfpos port new-chunk (- new-pos (* new-chunk 256))))))

            ((2) ; offset from the end of the file
             (set! #-of-bytes (min (abs #-of-bytes) file-size))
             (set! new-pos (- file-size (abs #-of-bytes))) ; absolute position
             (set! new-chunk (truncate (/ new-pos 256)))
             (set! new-pos (- newpos (* new-chunk 256))) ; buffer position
             (if (=? (%logand port-flags 3) 0)
                 (if (and (<? new-pos end-of-buffer)
                          (>=? new-pos 0)
                          (=? new-chunk current-chunk))
                     (%reify-port! port 9 new-pos)
                     (%sfpos port new-chunk new-pos))
                 (display "Offset from the end of the file can only be used with files open for reading!")
                 ))
            (else (display "Offset must be 0, 1 or 2!")))
          (display "First parameter must be a file!")))))

;******************************************************************
;* get-file-position will return the current file position in the *
;* number of bytes from the beginning of the file.                *
;******************************************************************

(define get-file-position
  (lambda (port)
    (let (( result '())
          (chunk (max 1 (%reify-port port 12))))
      (if (and (port? port)
               (=? (%logand (%reify-port port 11) 4) 0))
          (set! result (+ (* 256 (-1+ chunk)) ; chunk#
                          (%reify-port port 9)))        ; current position
          (set! result "Needs to be a port/file object!"))
      result)))

