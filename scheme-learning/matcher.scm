#lang scheme

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define (shows . s)
  (for-each display s)
  (newline))

(define (simplify-machine material)
  (let ((predicates (append (car material)
                          `((?a ,atom?)
                            (?p ,pair?))))
        (rules (cadr material)))
    (define (simplifier)
      (letrec
          ((simplify-exp
            (lambda (exp)
              (try-rules (if (pair? exp)
                             (map simplify-exp exp)
                             exp))))
           (try-rules
            (lambda (exp)
              (define (scan rules)
                (if (null? rules)
                    exp
                    (let ((dict (match (pattern-of (car rules))
                                  exp
                                  '())))
                      (cond
                        ((eq? dict 'failed)
                         ;; (shows "tried " (pattern-of (car rules)) " on " exp " whereas " 'failed)
                         (scan (cdr rules)))
                        (else
                         (shows exp #\newline
                                "|> " (pattern-of (car rules)) #\newline
                                "-> " (skeleton-of (car rules)) #\newline)
                         (simplify-exp
                          (instantiate
                              (skeleton-of (car rules))
                            dict)))))))
              (scan rules)))
            (pattern-of
             (lambda (rule)
               (car rule)))
            (skeleton-of
             (lambda (rule)
               (cadr rule))))
        simplify-exp))
    (define (match pat exp dict)
      (letrec
          ((arbitary-expression?
            (lambda (pat)
              (eq? (car pat) '?)))
           (one-of-these-predicates?
            (lambda (pat)
              (assq (car pat) predicates)))
           (the-predicate-yields-true?
            (lambda (pat exp)
              ((cadr (assq (car pat) predicates))
               exp))))
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
               dict))))))
    (define (extend-dict pat exp dict)
      (let ((name (cadr pat)))
        (let ((v (assq name dict)))
          (cond ((not v)
                 (cons (list name exp) dict))
                ((equal? (cadr v) exp) dict)
                (else 'failed)))))
    (define (instantiate skeleton dict)
      (letrec
          ((sub-instantiate
            (lambda (s)
              (cond
                ((atom? s) s)
                ((null? s) '())
                ((skeleton-evaluation? s)
                 (skeleton-evaluate (cadr s) dict))
                (else (cons (sub-instantiate (car s))
                            (sub-instantiate (cdr s)))))))
           (skeleton-evaluation?
            (lambda (s)
              (eq? (car s) ':)))
           (skeleton-evaluate
            (lambda (s dict)
              (if (atom? s)
                  (lookup s dict)
                  (eval
                   `(let ,(map (lambda (s)
                                 (list (car s)
                                       (list 'quote
                                             (cadr s))))
                               dict)
                      ,s)))))
           (lookup
            (lambda (s dict)
              (let ((t (assq s dict)))
                (if t
                    (cadr t)
                    s)))))
      (sub-instantiate skeleton)))
    (simplifier)))


;; --------------------------

(define algebra-rules
  '(((+ (?c c1) (?c c2)) (: (+ c1 c2)))
    ((* (?c c1) (?c c2)) (: (* c1 c2)))
    ((+ (?c c1) (+ (?c c2) (? e)))
     (+ (: (+ c1 c2)) (: e)))
    ((* (?c c1) (* (?c c2) (? e)))
     (* (: (* c1 c2)) (: e)))
    ((+ (? e) (? e)) (* 2 (: e)))
    ((+ (? e) (* (?c c) (? e)))
     (* (: (+ c 1)) (: e)))
    ((* (? e) (* (?c c) (? e)))
     (* (: (+ c 1)) (? e)))
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
    ;;; ^^ reversing
    ((+ 0 (? e)) (: e))
    ((* 1 (? e)) (: e))
    ((* 0 (? e)) 0)
    ((* (? a) (+ (? b) (? c)))
     (+ (* (: a) (: b)) (* (: a) (: c))))))

(define dsimp
  (simplify-machine
   (list
    `((?v ,symbol?)
      (?c ,number?))
    algebra-rules)))
