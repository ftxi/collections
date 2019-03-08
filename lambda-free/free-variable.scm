
;; require ski.scm

(define remove (lambda (x s)
                 (filter (lambda (u) (not (eqv? u x))) s)))
(define insert (lambda (x s)
                 (cons x (remove x s))))
(define is-in? (lambda (x s)
                 (cond
                   ((null? s) #f)
                   ((eqv? x (car s)) #t)
                   (else (is-in? x (cdr s))))))

(define (free-variable exp)
  (letrec
      ((fv (lambda (s)
             (let ((t (exp-type s)))
               (cond
                 ((eq? t '*expression-atom*)
                  (list (get-atom s)))
                 ((eq? t '*expression-application*)
                  (append (fv (get-operator s)) (fv (get-operand s))))
                 ((eq? t '*expression-lambda*)
                  (remove (get-lambda-variable s) (fv (get-lambda-body s))))
                 (else (error "[ERROR]free-variable: unexpected expression:" s)))))))
    (fv exp)))
