
;; logic.scm
;; mathematical logic analysis tools

;; to load the matcher
(load "matcher.scm")

;; logical operator: "if and only if" and "implies"
(define (iff a b)
  (or (and a b)
      (not (or a b))))

(define (implies a b)
  (or (not a)
      b))

;; utilities
(define (always-true? proc n-argum ents)
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

;; comparation tools, make it easier for simplification
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

;; the one-direction-simplifier
(define (simplifier rules)
  (letrec ((simplify-exp
            (lambda (exp)
              ; (shows exp)
              (pattern-transform*
               (if (list? exp)
                   (map simplify-exp exp)
                   exp)
               rules))))
    simplify-exp))

;; the simplifier that can simplify a single expression for multiple times
;; makes it possible to substitute a -> b, then b -> c
(define (multiple-simplifier rules)
  (letrec ((simplify-loop
            (lambda (exp tried)
              ;(shows "simplify: " exp)
              (if (member exp tried) ;; to avoid infinite loop
                  exp
                  (simplify-loop (pattern-transform*
                                  (if (list? exp)
                                      (map simplify-exp exp)
                                      exp)
                                  rules)
                                 (cons exp tried)))))
           (simplify-exp ;; to start a simplify loop
            (lambda (exp)
              (simplify-loop exp '()))))
    simplify-exp))
              

;; cannot use #t and #f, because they are self-evaluated the matcher use #f as the failed reply
(define :t #t)
(define :f #f)

(define (binary-boolean-operator? k)
  (or (eq? k 'and)
      (eq? k 'or)))

;; the logical simplification rules
(define logical-simplification-rules
  (matching-rules
   ;; drop constants
   (rule ()
         (not :t)
         ':f)
   (rule ()
         (not :f)
         ':t)
   (rule (p)
         (or :t p)
         ':t)
   (rule (p)
         (or :f p)
         p)
   (rule (p)
         (and :t p)
         p)
   (rule (p)
         (and :f p)
         ':f)
   (rule (k p)
         (k p p)
         p
         (binary-boolean-operator? k))
   (rule (k p q)
         (k p (k p q))
         `(,k ,p ,q)
         (binary-boolean-operator? k))
   (rule (p)
         (not (not p))
         p)
   ;; excluded middle
   (rule (p)
         (and p (not p))
         ':f)
   (rule (p)
         (or p (not p))
         ':t)
   ;; reversing, "k" is the connector
   (rule (k a b)
         (k a b)
         `(,k ,b ,a)
         (and (binary-boolean-operator? k)
              (logic-symbol-less? b a)))
   (rule (k a b c)
         (k a (k b c))
         `(,k ,b (,k ,a ,c))
         (and (logic-symbol-less? b a)
              (binary-boolean-operator? k)))
   ;; transfer to basic form
   (rule (a b)
         (implies a b)
         `(or (not ,a) ,b))
   (rule (a b)
         (iff a b)
         `(and (or (not ,a) ,b)
               (or (not ,b) ,a)))
   ;; De Morgan's rule
   (rule (p q)
         (not (or p q))
         `(and (not ,p) (not ,q)))
   (rule (p q)
         (not (and p q))
         `(or (not ,p) (not ,q)))
   ;; DNF (disjunctive normal form) transform
   (rule (a)
         (DNF-transform a)
         a
         (or (atom? a)
             (and (pair? a)
                  (eq? (car a) 'not))))
   (rule (a b c)
         (DNF-transform (and a (or b c)))
         `(or (CNF-transform (and ,a ,b))
              (CNF-transform (and ,a ,c))))
   (rule (k a b) ;; this match and the above one is in order, should not change
         (DNF-transform (k a b))
         `(,k (CNF-transform ,a)
              (CNF-transform ,b))
         (binary-boolean-operator? k))
   ;; CNF (conjunctive normal form) transform
   (rule (a)
         (CNF-transform a)
         a
         (or (atom? a)
             (and (pair? a)
                  (eq? (car a) 'not))))
   (rule (a b c)
         (CNF-transform (or a (and b c)))
         `(and (DNF-transform (or ,a ,b))
               (DNF-transform (or ,a ,c))))
   (rule (k a b) ;; in order
         (CNF-transform (k a b))
         `(,k (DNF-transform ,a)
              (DNF-transform ,b))
         (binary-boolean-operator? k))
   ))

(define lsimp (simplifier logical-simplification-rules))
(define (ldsimp x)
  ((multiple-simplifier logical-simplification-rules)
   (list 'DNF-transform x)))

(logic-symbol-less? '(not :f) ':t) ; #t
; (pattern-transform* '(and :t :f) logical-simplification-rules)
(lsimp '(and :t (or (not :t) :f))) ; :f
(lsimp '(implies p q)) ; (or (not p) q)
(ldsimp '(and (implies p q) (implies q p)))
(lsimp '(iff p q))
(ldsimp '(implies (implies (implies p q) p) p))
(ldsimp '(iff (iff p (not (not p))) (implies p p)))
#|
(lsimp '(and p (not p)))
(lsimp '(and (not p) p))
(lsimp '(and (and p q) r))
(lsimp '(and (or p q) r))
(ldsimp '(and (and t (and p r)) s))
(ldsimp '(and (and (not p) q) p))
|#
