#lang racket

(define-syntax err5
  (syntax-rules ()
    ((err5 s ...)
     (parameterize ((error-print-width 5))
       s ...))))

;(err5 (car (expt 10 1024)))

(define (err5f f)
  (parameterize ((error-print-width 5))
    (f)))

(err5f (lambda () (car (expt 10 1024))))