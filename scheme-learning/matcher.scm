#lang scheme

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define (match pat exp dict)
  (cond
    ((eq? dict 'failed) 'failed)
    ((atom? pat)
     (if (and (atom? exp)
              (eq? pat exp))
         dict
         'failed))
    ((null? pat) dict)
    ((null? exp) 'failed)
    ((arbitary-constant? pat)
     (if (constant? exp)
         (extend-dict pat exp dict)
         'failed))
    ((arbitary-variable? pat)
     (if (variable? exp)
         (extend-dict pat exp dict)
         'failed))
    ((arbitary-atom? pat)
     (if (atom? exp)
         (extend-dict pat exp dict)
         'failed))
    ((arbitary-pair? pat)
     (if (pair? exp)
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
       (skeleton-evaluate (cadr s) dict))
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

(define (simplifier the-rules)
  (define (simplify-exp exp)
    (try-rules (if (pair? exp)
                   (map simplify-exp exp)
                   exp)))
  (define (try-rules exp)
    (define (scan rules)
      (if (null? rules)
          exp
          (let ((dict (match (pattern-of (car rules))
                             exp
                             '())))
            (if (eq? dict 'failed)
                (scan (cdr rules))
                (simplify-exp
                 (instantiate
                     (skeleton-of (car rules))
                     dict))))))
    (scan the-rules))
  simplify-exp)

(define (arbitary-constant? pat)
  (eq? (car pat) '?c))

(define (constant? x)
  (number? x))

(define (arbitary-variable? pat)
  (eq? (car pat) '?v))

(define (variable? x)
  (and (atom? x)
       (not (number? x))))

(define (arbitary-atom? pat)
  (eq? (car pat) '?a))

(define (arbitary-pair? pat)
  (eq? (car pat) '?p))

(define (arbitary-expression? pat)
  (eq? (car pat) '?))

(define (skeleton-evaluation? s)
  (eq? (car s) ':))

(define (skeleton-evaluate s dict)
  (if (atom? s)
      (lookup s dict)
      'this-behavior-is-not-defined-yet))
                  
(define (lookup s dict)
  (let ((t (assq s dict)))
    (if t
        (cdr t)
        s)))

(define (pattern-of rule)
  (car rule))

(define (skeleton-of rule)
  (cadr rule))

;; --------------------------


(define derivative-rules
  '(((diff (?c c) (?v v)) 0)
    ((diff (?v v) (?v v)) 1)
    ((diff (?v u) (?v v)) 0)
    ((diff (+ (? x1) (? x2)) (?v v))
     (+ (diff (: x1) (: v)) (diff (: x2) (: v))))
    ((diff (* (? x1) (? x2)) (?v v))
     (+ (* (diff (: x1) (: v)) (: x2))
        (* (diff (: x2) (: v)) (: x1))))))

(define algebra-rules
  '(((+ (?p e) (?a a))
     (+ (: a) (: e)))
    ((* (?p e) (?a a))
     (* (: a) (: e)))
    ((+ 0 (? e)) (: e))
    ((* 1 (? e)) (: e))
    ((* 0 (? e)) 0)
    ((* (? a) (+ (? b) (? c)))
     (+ (* (: a) (: b)) (* (: a) (: c))))))

(define dsimp
  (simplifier (append algebra-rules
                      derivative-rules)))

