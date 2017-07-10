
(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define (shows . s)
  (for-each display s)
  (newline))

(define (simplify-machine material)
  (let ((the-predicates `(,@(car material)
                      (?a ,atom?)
                      (?p ,pair?)))
        (the-rules (cadr material)))
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
                         ;; (shows "tried " (pattern-of (car rules)) " on " exp " whereas " dict)
                         (scan (cdr rules)))
                        (else
                         (let ((v (andmap (lambda (r)
                                            (instantiate r dict))
                                          (skeleton-of (car rules)))))
                           (or v (shows exp #\newline
                                        "|> " (pattern-of (car rules)) #\newline
                                        "-> " (skeleton-of (car rules)) #\newline))
                           (or v (scan (cdr rules)))))))))
              (scan the-rules)))
            (pattern-of
             (lambda (rule)
               (car rule)))
            (skeleton-of
             (lambda (rule)
               (cdr rule))))
        simplify-exp))
    (define (match pat exp dict)
      (letrec
          ((arbitary-expression?
            (lambda (pat)
              (eq? (car pat) '?)))
           (one-of-these-predicates?
            (lambda (pat)
              (assq (car pat) the-predicates)))
           (the-predicate-yields-true?
            (lambda (pat exp)
              ((cadr (assq (car pat) the-predicates))
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
