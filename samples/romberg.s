;; $Id: romberg.s,v 1.3 2002/04/20 15:39:54 michab66 Exp $
;;
;; (romberg func a b)
;;
;; Integrate func in the integration limits a b using the Romberg algorithm.

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (romberg func a b i)

  (letrec ((t00 (/ (* (- b a) (+ (func a) (func b))) 2))

           (ti0 (lambda (zeile)
                  (if (zero? zeile)
                      t00
                      (letrec ((e2i (expt 2 zeile))
                               (e2im1 (expt 2 (dec zeile)))
                               (sum (lambda (k)
                                      (if (<= k 0)
                                          0
                                          (+ (func (/ (+ (* a (inc (- e2i (* 2 k))))
                                                         (* b (dec (* 2 k))))
                                                      e2i))
                                             (sum (dec k)))))))

                        (* (/ 1.0 e2i)
                           (+ (* e2im1 (ti0 (dec zeile)))
                              (* (- b a) (sum e2im1))))))))

           (tii (lambda (zeile spalte)
                  (if (zero? spalte)
                      (ti0 zeile)
                      (let* ((p (expt 4 spalte))
                             (pm1 (dec p)))

                        (/ (- (* p (tii zeile (dec spalte)))
                              (tii (dec zeile) (dec spalte)))
                           pm1))))))

    (tii i i)))


(define (romberg-test func a b)
  (do ((i 1.0 (inc i)))
      ((> i 5) i)

      (display "T( ") (display i) (display ", ") (display i) (display " ) = ")
      (display (romberg func a b i)) (newline)))
