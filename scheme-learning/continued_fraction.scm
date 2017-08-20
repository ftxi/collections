(load "stream.scm")

(define (continued-fraction n)
  (let* ((m (inexact->exact (floor n)))
         (p (- n m)))
    (if (= p 0)
        (lazy-cons m '())
        (lazy-cons m (continued-fraction (/ p))))))

(define (approximate k n)
  (let ((rcf (reverse (take k (continued-fraction n)))))
    (let loop ((acc (car rcf)) (l (cdr rcf)))
      (if (null? l)
          acc
          (loop (+ (/ acc) (car l)) (cdr l))))))

(define (gradually-approximate k n)
  (let loop ((u k) (ans '()))
    (if (= u 0)
        ans
        (loop (- u 1)
              (cons (approximate u n) ans)))))

;;; some useful constants
(define e (exp 1))
(define pi (* 4 (atan 1)))
