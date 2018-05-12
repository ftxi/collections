
;;   (lambda (a b c) body)
;;-> (λ (a) (λ (b) (λ (c) body)))

(define-syntax lambda-curry
  (syntax-rules ()
    ((lambda-curry (x xs ...) body)
     (lambda (x) (lambda-curry (xs ...) body)))
    ((lambda-curry () body)
     body)))

;;   (apply-curry f x y z)
;;-> (((f x) y) z)

(define-syntax apply-curry
  (syntax-rules ()
    ((apply-curry f x)
     (f x))
    ((apply-curry f x xs ...)
     (apply-curry (f x) xs ...))
    ((apply-curry x)
     x)))

;;   (apply-curry* (f (g h i) l m))
;;-> (apply-curry f (apply-curry g h i) l m)
;;-> (((f ((g h) i)) l) m)

(define-syntax apply-curry*
  (syntax-rules ()
    ((apply-curry* (f xs ...))
     (apply-curry* "step" f (xs ...) ()))
    ((apply-curry* "step" f ((g ys ...) xs ...) us)
     (apply-curry* "step" f (xs ...) ((apply-curry* (g ys ...)) . us)))
    ((apply-curry* "step" f (x xs ...) us)
     (apply-curry* "step" f (xs ...) (x . us)))
    ((apply-curry* "step" f () us)
     (apply-curry* "reverse" f () us))
    ((apply-curry* "reverse" f xs (u . us))
     (apply-curry* "reverse" f (u . xs) us))
    ((apply-curry* "reverse" f xs ())
     (apply-curry f . xs))
    ((apply-curry* x)
     x)))

(define-syntax define-curried-function
  (syntax-rules ()
    ((define-curried-function (f args ...) body)
     (define f (lambda-curry (args ...) (apply-curry* body))))))

(define-curried-function (I x)
  x)

(define-curried-function (K x y)
  x)

(define-curried-function (S x y z)
  (x z (y z)))

(define-syntax %eq?
  (syntax-rules ()
    ((%eq? a b)
     (let-syntax ((f (syntax-rules (a)
                       ((f a) #t)
                       ((f _) #f))))
       (f b)))))














