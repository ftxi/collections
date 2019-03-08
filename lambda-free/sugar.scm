
;; sugar.scm
;; compile lisp-like programs to pure lambda calculi

;; support:
;; if, cond, let, letrec, named-let, car, cdr, null?
;; 
;; booleans using k and `ki, i.e. λp.λq.p and λp.λq.q

(load "ski.scm")

;; some constants

(define true (list->expression '(λ (p) (λ (q) p))))
(define false (list->expression '(λ (p) (λ (q) q))))

(define nil (list->expression '(λ (x) (λ (p) (λ (q) p)))))
(define null (list->expression '(λ (g) (g (λ (x) (λ (y) (λ (p) (λ (q) q))))))))


