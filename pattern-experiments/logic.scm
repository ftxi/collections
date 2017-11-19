
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


;; cannot use #t and #f, because they are self-evaluated the matcher use #f as the failed reply
(define :t #t)
(define :f #f)

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
   ;; reversing
   (rule (a b)
         (a b)
         `(,b ,a)
         (logic-symbol-less? b a))
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
   ;(rule (a b c)
   ;      (and a (or b c))
   ;      `(or (and ,a ,b) (and ,a ,c)))
   ))

(define lsimp (simplifier logical-simplification-rules))

(logic-symbol-less? '(not :f) ':t)
; (pattern-transform* '(and :t :f) logical-simplification-rules)
(lsimp '(and :t (or (not :t) :f)))
(lsimp '(implies p q))
(lsimp '(and (implies p q) (implies q p)))
(lsimp '(iff p q))
(lsimp '(implies (implies (implies p q) p) p))










