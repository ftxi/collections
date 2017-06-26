#lang scheme

(define (fact-set n)
  (let ((ans 1))
    (define (loop)
      (cond
        ((= n 1) ans)
        (else (set! ans (* ans n))
              (set! n (- n 1))
              (loop))))
    (loop)))

(define square
  (lambda (x)
    (* x x)))

(define (average . s)
  (/ (eval (cons '+ s))
     (length s)))

(define (-sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))


