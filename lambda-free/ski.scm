
(load "utilities.scm")

(define remove (lambda (x s)
                 (filter (lambda (u) (not (eqv? u x))) s)))
(define insert (lambda (x s)
                 (cons x (remove x s))))
(define is-in? (lambda (x s)
                 (cond
                   ((null? s) #f)
                   ((eqv? x (car s)) #t)
                   (else (is-in? x (cdr s))))))

(define (make-atom a)
  (list '*expression-atom* a))

(define (make-application g h)
  (list '*expression-application* g h))

(define (make-lambda x f)
  (list '*expression-lambda* x f))

(define exp-type car)
(define get-atom cadr)
(define get-operator cadr)
(define get-operand caddr)
(define get-lambda-variable cadr)
(define get-lambda-body caddr)

(define (list->expression l)
  (cond
    ((list? l)
     (cond
       ((null? (cdr l))
        (error "[ERROR]list->expression: too few entries in" l))
       ((null? (cddr l))
        (make-application
         (list->expression (car l))
         (list->expression (cadr l))))
       ((and (eq? (car l) 'λ)
             (list? (cadr l))
             (atom? (caadr l))
             (null? (cdadr l))
             (not (null? (cddr l)))
             (null? (cdddr l)))
        (make-lambda (caadr l) (list->expression (caddr l))))
       (else (error "[ERROR]list->expression: unexpected entry:" l))))
    ((null? l) (error "[ERROR]list->expression: unexpected nil:" l))
    (else (make-atom l))))

(define (expression->list s)
  (let ((t (exp-type s)))
    (cond
      ((eq? t '*expression-atom*)
       (get-atom s))
      ((eq? t '*expression-application*)
       (list (expression->list (get-operator s)) (expression->list (get-operand s))))
      ((eq? t '*expression-lambda*)
       `(λ (,(get-lambda-variable s)) ,(expression->list (get-lambda-body s))))
      (else (error "[ERROR]expression->list: unexpected expression:" s)))))

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

(define (eliminate e)
  (let ((t (exp-type e)))
    (cond
      ((eq? t '*expression-atom*)
       e)
      ((eq? t '*expression-application*)
       (make-application (eliminate (get-operator e))
                         (eliminate (get-operand e))))
      ((eq? t '*expression-lambda*)
       (let* ((x (get-lambda-variable e))
              (f (get-lambda-body e))
              (f0 (eliminate f))
              (t0 (exp-type f0)))
         (if (is-in? x (free-variable f))
             (cond
               ((eq? t0 '*expression-atom*)
                (make-atom '$i))
               ((eq? t0 '*expression-application*)
                (make-application
                 (make-application (make-atom '$s)
                                   (eliminate (make-lambda x (get-operator f0))))
                 (eliminate (make-lambda x (get-operand f0)))))
               (else
                (error "[ERROR]eliminate: unexpected expression during lambda elimination:" (expression->list f))))
             (make-application (make-atom '$k) f0))))
      (else (error "[ERROR]eliminate: unexpected expression:" e)))))

(define (unlambda-style s)
  (let ((t (exp-type s)))
    (cond
      ((eq? t '*expression-atom*)
       (let ((s0 (get-atom s)))
         (cond
           ((eq? s0 '$s) "s")
           ((eq? s0 '$k) "k")
           ((eq? s0 '$i) "i")
           (else (symbol->string s0)))))
      ((eq? t '*expression-application*)
       (string-append "`" (unlambda-style (get-operator s)) "" (unlambda-style (get-operand s))))
      ((eq? t '*expression-lambda*)
       (string-append "^" (symbol->string (get-lambda-variable s)) "." (unlambda-style (get-lambda-body s))))
      (else (error "[ERROR]unlambda-style: unexpected expression:" s)))))

(define (show-unlambda s)
  (shows (unlambda-style s)))

;;;;;;;;;;;;;;;;;;;;;;
;; ------test------ ;;
;;;;;;;;;;;;;;;;;;;;;;

(define s (list->expression '(λ (x)
                               (λ (y)
                                 (λ (z)
                                   ((x z) (y z)))))))
(define k (list->expression '(λ (x) (λ (y) x))))
(define i (list->expression '(λ (y) y)))
(define one (list->expression '(λ (f) (λ (x) (f x)))))
(define loop (list->expression '((λ (x) (x x)) (λ (x) (x x)))))

s

(show-unlambda s)

(expression->list s)

(free-variable s)
(free-variable (get-lambda-body s))
(show-unlambda (eliminate s))
(show-unlambda (eliminate k))
(show-unlambda (eliminate i))
(show-unlambda (eliminate one))
(show-unlambda (eliminate (list->expression '(λ (x) (f x)))))
(show-unlambda (eliminate loop))
