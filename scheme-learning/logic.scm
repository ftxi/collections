#lang scheme/load

(load "matcher.scm")

(define (iff a b)
  (or (and a b)
      (not (or a b))))

(define (implies a b)
  (or (not a)
      b))

(define (always-true? proc n-arguments)
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

;;  u  &  v -> #f
;;  u  & ~v -> #f
;; ~u  &  v -> #t
;; ~u  & ~v -> #f

(define logical-material
  `(((?v ,symbol?)
     (?l ,(lambda (s)
            (or (eq? s 'and)
                (eq? s 'or)
                (eq? s 'not)
                (eq? s '->)
                (eq? s '<->)))))
    (((and (? a) (? b))
      (: (logic-symbol-less? b a))
      (and (: b) (: a)))
     ((implies (? a) (? b))
      (and (not a) b))
     ((iff (? a) (? b))
      (or (and (: a) (: b))
          (and (not (: a))
               (not (: b)))))
     
     ; ... and more and more ...
     ((not (not (? e)))
      (: e)))))

(define lsimp
  (simplify-machine logical-material))

(lsimp '(iff a b))