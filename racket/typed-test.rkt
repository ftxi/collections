#lang typed/racket

(: sum-list ((Listof Number) . -> . Number))
(define (sum-list l)
  (cond [(null? l) 0]
        [else (+ (car l) (sum-list (cdr l)))]))

(sum-list (list 1 2 3))