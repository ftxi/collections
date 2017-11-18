
;; matcher-version4.scm
;; the version 4 of my own pattern matcher

;; the atom predicate, I don't understand why this is not given by default

(define (atom? p)
  (and (not (pair? p))
       (not (null? p))))

;; the matching-rule tuple structure

(define (make-matching-rule-tuple vars pat skt preq)
  (list vars pat skt preq))

(define (variables-of rule)
  (car rule))

(define (pattern-of rule)
  (cadr rule))

(define (skeleton-of rule)
  (caddr rule))

(define (prerequisite-of rule)
  (cadddr rule))

;; create a matching-rules macro to abbrivate the syntax
;; (matching-rules rule1 rule2 ...)
;; where each rule is (vars pat skt preq)
;;  - vars: the variables in pat
;;  - pat:  the pattern to be matched
;;  - skt:  the skeleton used for substitution
;;  - preq: the prerequisite of matching, which determines whether a matching may success
;;          default returns #t anyway

(define-syntax matching-rules
  (syntax-rules (rule)
    ((matching-rules "single_line" vars pat skt preq)
     (make-matching-rule-tuple
      'vars
      'pat
      (lambda vars
        skt)
      (lambda vars
        preq)))
    ((matching-rules "single_line" vars pat skt)
     (make-matching-rule-tuple
      'vars
      'pat
      (lambda vars
        skt)
      (lambda vars
        #t)))
    ((matching-rules) '())
    ((matching-rules (rule vars ...) e2 ...)
     (cons (matching-rules "single_line" vars ...)
           (matching-rules e2 ...)))
    ((matching-rules (vars ...) e2 ...)
     (cons (matching-rules "single_line" vars ...)
           (matching-rules e2 ...)))))

;(matching-rules "single_line" (a b) (+ a b) (+ a b) (and (number? a) (number? b)))
(matching-rules ((a b) (+ a b) (+ a b) (and (number? a) (number? b)))
                ((a b) (+ a b) `(+ ,a ,b)))

;; a few searching utilities
;; a dict is something like ((a) (b . 3) (c))
;; in the above case, a and c is not matched yet 

(define (reassoc obj val alist)
  (map (lambda (x)
         (if (eq? (car x) obj)
             (cons obj val)
             x))
       alist))

(reassoc 'b 3 (map list '(a b c)))

;; the pattern-transformer
;; returns the substituted skeleton if success, or #f if fail
(define (pattern-transform exp rule)
  (let ((vars (variables-of rule))
        (pat (pattern-of rule))
        (skt (skeleton-of rule))
        (preq (prerequisite-of rule)))
    (let ((dict (matching-values pat exp (map list vars))))
      (and dict ;; if the dict is not empty...
           (let ((operands (map cdr dict)))
             (and (apply preq operands) ;; if the prerequisites are fulfilled...
                  (apply skt operands)))))))

;; the pattern matcher
;; return the a dictionary in which values are associated, or #f if fail
(define matching-values
  (lambda (pat exp dict)
    (cond
      ((not dict) #f) ;; passing failures outside
      ((null? pat) (and (null? exp) dict))
      ((null? exp) #f)
      ((atom? pat)
       (let ((res (assoc pat dict)))
         (cond
           ((not res) ;; there is no such symbol in that dictionary
            (and (eqv? pat exp) dict)) ;; i.e. the expression has to be a specific one
           ((null? (cdr res)) ;; we do not know what pattern stands for yet
            (reassoc pat exp dict))
           ((equal? exp (cdr res)) dict) ;; the expression is just like what it should be like
           (else #f))))
      ((atom? exp) #f)
      (else ;; otherwise recursively run matching on the car-part and cdr-part
       (matching-values (cdr pat)
                        (cdr exp)
                        (matching-values (car pat)
                                         (car exp)
                                         dict))))))
;; some tests
(matching-values '(+ (* a b) b) '(+ (* 1 x) x) '((a) (b)))

(pattern-transform '(+ 10 0)
                   (car (matching-rules
                         ((a)
                          (+ a 0)
                          a)))) ; => 10

(pattern-transform '(* 2 (+ u v))
                   (matching-rules "single_line"
                                   (a b c)
                                   (* a (+ b c))
                                   `(+ (* ,a ,b)
                                       (* ,a ,c)))) ; => (+ (* 2 u) (* 2 v))

;; multiple pattern transformer
;; return the expression itself if no rules are matched
(define (pattern-transform* exp rules)
  (if (null? rules)
      exp
      (let ((res (pattern-transform exp (car rules))))
        (or res
            (pattern-transform* exp (cdr rules))))))

;; do some tests
(define rules
  (matching-rules
   (rule (a b)
         (+ a b)
         (+ a b)
         (and (number? a)
              (number? b)))
   (rule (a)
         (+ a 0)
         a)
   (rule (a b c)
         (* a (+ b c))
         `(+ (* ,a ,b)
             (* ,a ,c)))))

(pattern-transform* '(+ 1 2) rules) ; => 3
(pattern-transform* '(+ c 0) rules) ; => c
(pattern-transform* '(* k (+ s t)) rules) ; => (+ (* k s) (* k t))
(pattern-transform* '(* p q) rules) ; => (* p q)
