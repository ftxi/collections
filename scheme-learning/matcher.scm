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

(define (instantiate form dict)
  (eval
   `(let ,(map (lambda (s)
                 (list (car s)
                       (list 'quote
                             (cadr s))))
               dict)
      ,form)))

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
                    (and ans (shows "use " (caar rules) " => " (cadar rules) " on " exp " , get " ans))
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

(define (simple-simplifier the-rules)
  (define (simplify-exp exp)
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
                    (and ans (shows "use " (caar rules) " => " (cadar rules) " on " exp " , get " ans))
                    (if ans
                        ans
                        (scan (cdr rules))))))))
      (scan the-rules))
    (try-rules (if (pair? exp)
                   (map initial-simplify-exp exp)
                   exp)))
  (define (initial-simplify-exp exp)
    (simplify-exp exp))
  initial-simplify-exp)

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

;; ------------------------------------

(define (logic-varible? s)
  (and (pair? s)
       (and (eq? (car s) 'not)
            (null? (cdr s)))))

(define (logic-pair? s)
  (and (pair? s)
       (not (logic-varible? s))))

(define the-predicates
  `((?a ,atom?)
    (?p ,pair?)
    (?c ,number?)
    (?v ,symbol?)
    (?lv ,logic-varible?)
    (?lp ,logic-pair?)
    (? ,(lambda (s) #t))))

;; ------------------------------------


(define (symbol-less? u v)
  (let ((atom->string
         (lambda (a)
           (cond
             ((number? a) (number->string a))
             ((symbol? a) (symbol->string a))))))
    (cond
      ((and (atom? u)
            (atom? v))
       (string<? (atom->string u)
                 (atom->string v)))
      ((and (pair? u)
            (pair? v))
       (if (equal? (car u) (car v))
           (symbol-less? (cdr u) (cdr v))
           (symbol-less? (car u) (car v))))
      ((null? v) #f)
      ((null? u) #t)
      ((atom? v) #f)
      ((atom? u) #t))))

(define (logic-symbol-less? u v)
  (let* ((u-not?
          (and (pair? u)
               (eq? (car u) 'not)))
         (v-not?
          (and (pair? v)
               (eq? (car v) 'not)))
         (u-s (if u-not?
                  (cadr u)
                  u))
         (v-s (if v-not?
                  (cadr v)
                  v)))
    (if (equal? u-s v-s)
        (and (not u-not?) v-not?)
        (symbol-less? u-s v-s))))

(define logic-rules
  '(((and (? a) (? b))
     (and (logic-symbol-less? b a)
          `(and ,b ,a)))
    ((or (? a) (? b))
     (and (logic-symbol-less? b a)
          `(or ,b ,a)))
    ((and (?p p) (?a a))
     `(and ,a ,p))
    ((or (?p p) (?a a))
     `(or ,a ,p))
    ((and (and (? a) (? b)) (? c))
     `(and ,a (and ,b ,c)))
    ((or (or (? a) (? b)) (? c))
     `(or ,a (or ,b ,c)))
    ((and (? a) (and (? b) (? c)))
     (and (logic-symbol-less? b a)
          `(and ,b (and ,a ,c))))
    ((or (? a) (or (? b) (? c)))
     (and (logic-symbol-less? b a)
          `(or ,b (or ,a ,c))))
    ;; definations
    ((implies (? a) (? b))
     `(or (not ,a) ,b))
    ((iff (? a) (? b))
     `(or (and ,a ,b)
         (and (not ,a) (not ,b))))
    ;; move not inside & erase double not(s)
    ((not (and (? a) (? b)))
     `(or (not ,a) (not ,b)))
    ((not (or (? a) (? b)))
     `(and (not ,a) (not ,b)))
    ((not (not (? p)))
     p)
    ;; logical identity
    ((and (? p) (not (? p)))
     'false)
    ((and (? p) (and (not (? p)) (? q)))
     'false)
    ((or (? p) (not (? p)))
     'true)
    ((or (? p) (or (not (? p)) (? q)))
     q)
    ((and (? p) true)
     p)
    ((and (? p) false)
     'false)
    ((or (? p) true)
     'true)
    ((or (? p) false)
     'false)
    ((and true (? p))
     p)
    ((and false (? p))
     'false)
    ((or true (? p))
     'true)
    ((or false (? p))
     p)))

(define de-morgan-rules
  '(((and (? a) (or (? b) (? c)))
     `(or (and ,a ,b) (and ,a ,c)))
    ((or (? a) (and (? b) (? c)))
     `(and (or ,a ,b) (or ,a ,c)))))

(define loopsimp
  (simple-simplifier '(((foo (? a) (? b))
                 `(foo ,b ,a)))))

(define lsimp
  (simplifier logic-rules))

(define dsimp
  (simple-simplifier de-morgan-rules))

(define (nsimp x)
  (lsimp (dsimp (lsimp x))))

;(lsimp '(and b a))
