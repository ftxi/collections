#lang scheme

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define deriv-rules
  '(
    ((dd (?c c) (?v v)) 0)
    ((dd (?v v) (?v v)) 1)
    ((dd (?v u) (?v v)) 0)
    ((dd (+ (? x1) (? x2)) (?v v))
     (+ (dd (: x1) (: v)) (dd (: x2) (: v))))
    ((dd (* (? x1) (? x2)) (?v v))
     (+ (* (dd (: x1) (: v)) (: x2))
        (* (dd (: x2) (: v)) (: x1))))))

#|
(define algebra-rules
  '(
    (((? op) (?c c1) (?c c2)) (: (op c1 c2)))
    (((? op) (? e) (?c c)) ((: op) (: c) (: e)))
|#

(define (match pat exp dict)
  (cond
    ((eq? dict 'failed) 'failed)
    ((atom? pat)
     (if (and (atom? exp)
              (eq? pat exp))
         dict
         'failed))
    ((null? pat)
     dict)
    ((arbitary-constant? pat)
     (if (constant? exp)
         (extend-dict pat exp dict)
         'failed))
    ((arbitary-variable? pat)
     (if (variable? exp)
         (extend-dict pat exp dict)
         'failed))
    ((arbitary-expression? pat)
     (extend-dict pat exp dict))
    ((atom? exp)
     'failed)
    (else
     (match (cdr pat)
            (cdr exp)
            (match (car pat)
                   (car exp)
                   dict)))))

(define (instantiate skeleton dict)
  (define (sub-instantiate s)
    (cond
      ((atom? s) s)
      ((null? s) '())
      ((skeleton-evaluation? s)
       (skeleton-evaluate (eval-exp s) dict))
      (else (cons (sub-instantiate (car s))
                  (sub-instantiate (cdr s))))))
  (sub-instantiate skeleton))

(define (extend-dict pat exp dict)
  (let ((name (cadr pat)))
    (let ((v (assq name dict)))
      (cond ((not v)
             (cons (cons name exp) dict))
            ((equal? (cdr v) exp) dict)
            (else 'failed)))))

(define (arbitary-constant? pat)
  (eq? (car pat) '?c))

(define (constant? x)
  (number? x))

(define (arbitary-variable? pat)
  (eq? (car pat) '?v))

(define (variable? x)
  (and (atom? x)
       (not (number? x))))

(define (arbitary-expression? pat)
  (eq? (car pat) '?))

(define (skeleton-evaluation? s)
  (eq? (car s) ':))

(define (eval-exp s)
  (cadr s))

(define (skeleton-evaluate s dict)
  (if (atom? s)
      (lookup s dict)
      'this-behavior-is-not-defined-yet))
                  
(define (lookup s dict)
  (let ((t (assq s dict)))
    (if t
        (cdr t)
        s)))


