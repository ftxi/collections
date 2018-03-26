
(define-syntax syntax-error
  (syntax-rules ()
    ((syntax-error)
     (syntax-error "[Error]Bad use of syntax-error."))))

(define-syntax nth-value
  (syntax-rules ()
    ((_ n values-producing-form)
     (call-with-values                    ;; Note the quote!
       (lambda () values-producing-form)
       (lambda all-values
         (list-ref all-values n))))))

(nth-value 1 (values 1 2 3))

(define-syntax please
  (syntax-rules ()
    ((please function . form) (function . form))))

(please + 1 2)

#|

(define-syntax named-let
  (syntax-rules ()
    ((named-let () . rest)
     (syntax-error "name must be a symbol instead of " ()))
    ((named-let (car . cdr) . rest)
     (syntax-error "name must be a symbol instead of " (car . cdr)))
    ((named-let name ((k1 v1) . more-bindings) . body)
     (named-let "step" name (k1) (v1) more-bindings body))
    ((named-let "step" name (k1 . ks) (v1 . vs) ((k2 v2) . more-bindings) body)
     (named-let "step" name (k2 k1 . ks) (v2 v1 . vs) more-bindings body))
    ((named-let "step" name (k . ks) (v . vs) () body)
     (((lambda (tmp)
         (lambda (name)
           (tmp tmp)))
       (lambda (rec)
         (lambda ks)))
      vs))))


(named-let name ((a 'b) (c 'd)) (cons a c))
|#


