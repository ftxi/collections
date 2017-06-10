#lang scheme

(define (even? x)
  (eq? (remainder x 2) 0))

(define (square x)
  (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n a)
  (= (expmod a n n) a))

(define (prime?-fast n times)
  (cond
    ((= times 0) #t)
    ((fermat-test n (min 1048575 (random n)))
     (prime?-fast n (- times 1)))
    (else #f)))

(define (large-number)
  (let loop ((n 1) (ans 0))
    (if (> n 50)
        ans
        (loop (+ n 1) (+ (* ans 100) (random 100))))))

(define (large-prime-number)
  (let ((x (large-number)))
    (let loop ((n 1))
      (cond
        ((> n 100) x)
        ((fermat-test x (random 1048575)) (loop (+ n 1)))
        (else (large-prime-number))))))
        
