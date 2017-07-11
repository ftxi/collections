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
    ((and (null? pat)
          (null? exp))
     dict)
    ((null? pat) 'failed)
    ((null? exp) 'failed)
    ((one-of-these-predicates? pat)
           (if (the-predicate-yields-true? pat exp)
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

(define one-of-these-predicates?
 (lambda (pat)
   (assq (car pat) the-predicates)))

(define the-predicate-yields-true?
  (lambda (pat exp)
    ((cadr (assq (car pat) the-predicates))
     exp)))

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
             (cons (list name exp) dict))
            ((equal? (cadr v) exp) dict)
            (else 'failed)))))

(define (simplifier the-rules)
  (define (simplify-exp exp used)
    (define (try-rules exp)
      (define (scan rules)
        (if (null? rules)
            exp
            (let ((dict (match (pattern-of (car rules))
                          exp
                          '())))
              (if (eq? dict 'failed)
                  (scan (cdr rules))
                  (let ((ans (instantiate
                                 (skeleton-of (car rules))
                               dict)))
                    (and ans (shows "using " (caar rules) " => " (cadar rules) " on " exp " gets " ans))
                    (if ans
                        (simplify-exp ans (cons exp used))
                        (scan (cdr rules))))))))
      (scan the-rules))
    (if (ormap (lambda (s)
                 (equal? s exp))
               used)
        exp
        (try-rules (if (pair? exp)
                       (map initial-simplify-exp exp)
                       exp))))
  (define (initial-simplify-exp exp)
    (simplify-exp exp '()))
  initial-simplify-exp)

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
      (eval
       `(let ,(map (lambda (s)
                     (list (car s)
                           (list 'quote
                                 (cadr s))))
                   dict)
          ,s))))

(define (lookup s dict)
  (let ((t (assq s dict)))
    (if t
        (cadr t)
        s)))

(define (pattern-of rule)
  (car rule))

(define (skeleton-of rule)
  (cadr rule))

(define (shows . s)
  (for-each display s)
  (newline))

(define the-predicates
  `((?a ,atom?)
    (?p ,pair?)
    (?c ,number?)
    (?v ,symbol?)))

(define algebra-rules
  '(((+ (?c c1) (?c c2))
     (: (if (> c1 c2)
            `(*add* ,c2 ,c1)
            (+ c1 c2))))
    ((* (?c c1) (?c c2)) (: (* c1 c2)))
    ((- (?c c)) (: (- c)))
    ((expt (?c c1) (?c c2)) (: (expt c1 c2)))
    ((* (? e) (expt (? e) (? k)))
     (expt (: e) (+ 1 (: k))))
    ((+ (?c c1) (+ (?c c2) (? e)))
     (+ (: (+ c1 c2)) (: e)))
    ((* (?c c1) (* (?c c2) (? e)))
     (* (: (* c1 c2)) (: e)))
    ((+ (? e) (? e)) (* 2 (: e)))
    ((+ (? e) (* (?c c) (? e)))
     (* (: (+ c 1)) (: e)))
    ((* (? e) (? e)) (expt (: e) 2))
    ((* (? e) (* (?c c) (? e)))
     (* (: (+ c 1)) (: e)))
    ;;; ^^ constants
    ((+ (?p e) (?a a))
     (+ (: a) (: e)))
    ((* (?p e) (?a a))
     (* (: a) (: e)))
    ((+ (?v v) (?c c))
     (+ (: c) (: v)))
    ((* (?v v) (?c c))
     (* (: c) (: v)))
    ((+ (+ (? a) (? b)) (? c))
     (+ (: a) (+ (: b) (: c))))
    ((* (* (? a) (? b)) (? c))
     (* (: a) (* (: b) (: c))))
    ((+ (?p e1) (+ (?c c) (? e2)))
     (+ (: c) (+ (: e2) (: e1))))
    ((- (? e1) (? e2))
     (+ (: e1) (- (: e2))))
    ;;; ^^ reversing
    ((+ 0 (? e)) (: e))
    ((* 1 (? e)) (: e))
    ((* 0 (? e)) 0)
    ((expt (? e) 0) 1)
    ((expt (? e) 1) (: e))
    ((expt (expt (? e) (? e1)) (? e2))
     (expt (: e) (* (: e1) (: e2))))
    ((* (expt (? e) (? e1))
        (expt (? e) (? e2)))
     (expt (: e) (+ (: e1) (: e2))))
    ((* (? a) (+ (? b) (? c)))
     (+ (* (: a) (: b)) (* (: a) (: c))))))

(define dsimp
  (simplifier algebra-rules))