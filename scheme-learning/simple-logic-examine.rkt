#lang scheme

(define (<-> a b)
  (or (and a b)
      (not (or a b))))

(define (-> a b)
  (or (not a)
      b))

(define (always-true? proc n-arguments)
  (letrec
      ((rec (lambda (args n)
              (cond
                ((= n 0)
                 (apply proc args))
                (else
                 (and (rec (cons #t args) (- n 1))
                      (rec (cons #f args) (- n 1))))))))
    (rec '() n-arguments)))

(define (true-value-table proc n-arguments)
  (letrec
      ((rec (lambda (args n)
              (cond
                ((= n 0)
                 (display args)
                 (display " => ")
                 (display (apply proc args))
                 (newline))
                (else
                 (rec (cons #t args) (- n 1))
                 (rec (cons #f args) (- n 1)))))))
    (rec '() n-arguments)))

(define alpha
  (lambda (p q r)
    (<-> (<-> (<-> p q) r)
         (<-> p (<-> q r)))))
