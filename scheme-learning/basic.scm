#lang scheme

((lambda (:cons :car :cdr)
   (:car (:cons (quote apple) (quote banana))))
 (lambda (a b)
    (lambda (m)
      (m a b)))
 (lambda (s)
    (s (lambda (a b)
         a)))
 (lambda (s)
    (s (lambda (a b)
         b))))

;; -> apple

#|

(define :cons
  (lambda (a b)
    (lambda (m)
      (m a b))))

(define :car
  (lambda (s)
    (s (lambda (a b)
         a))))

(define :cdr
  (lambda (s)
    (s (lambda (a b)
         b))))

|#



