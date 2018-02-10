
;; lambda.scm
;; perform lambda calculus using pattern matching

(load "matcher.scm")

(define (distinguishable-name names)
  (let ((chars (map (compose string->list
                             symbol->string)
                    names)))
    ((compose string->symbol list->string)
     (let loop ((cs chars) (ans '(#\a)))
       (if (null? cs)
           ans
           (loop (cdr cs)
                 (let ((m (length (car cs)))
                       (n (length ans)))
                   (cond
                     ((< m n) (loop (cdr cs) ans))
                     ((= m n)
                      (if (equal? (car cs) ans)
                          (append ans (list #\1))
                          ans))
                     (else
                      (if (equal? (take n (car cs)) ans)
                          (append ans
                                  (list (different-char (list-ref n (car cs)))))
                          ans))))))))))

(define (different-char x)
  (let ((ci char->integer)
        (ic integer->char))
    (cond
      ((<= (ci #\0) (ci x) (ci #\8))
       (ic (+ (ci x) 1)))
      ((= (ci #\9) (ci x))
       #\0)
      ((<= (ci #\a) (ci x) (ci #\y))
       (ic (+ (ci x) 1)))
      ((= (ci #\z) (ci x))
       #\a)
      ((<= (ci #\A) (ci x) (ci #\Y))
       (ic (+ (ci x) 1)))
      ((= (ci #\Z) (ci x))
       #\A)
      (else #\0))))

(define reduction-rules
  (matching-rules
   ;; eta-reduction
   (rule (x s)
         (λ (x) (s x))
         s)
   ;; beta-reduction
   (rule (p q s)
         ((λ (p) s) q)
         `((substitute ,p ,q) ,s))
   (rule (p q)
         ((substitute p q) p)
         q)
   (rule (p q r)
         ((substitute p q) r)
         r)
   (rule (p q s)
         ((substitute p q) (λ (p) s))
         `(λ (,p) ,s))
   
   ))

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

(define λ-simp (simplifier reduction-rules))

(define (λ-show exp)
  (pattern-transform*
   exp
   (matching-rules
    (rule (x s)
          (λ (x) s)
          (begin (display "(λ ")
                 (display x)
                 (display #\.)
                 (λ-show s)
                 (display #\))))
    (rule (p q)
          (p q)
          (begin (display #\()
                 (λ-show p)
                 (display #\space)
                 (λ-show q)
                 (display #\))))
    (rule (a)
          a
          (display a)))))

(λ-show (λ-simp (λ-simp '(λ (x) ((λ (y) y) x)))))











         
