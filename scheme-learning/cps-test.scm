
(define (k-style operator)
  (lambda arglis
    (let* ((revlis (reverse arglis))
           (k (car revlis))
           (operands (reverse (cdr revlis))))
      (k (apply operator operands)))))

(define id
  (lambda (x)
    x))

(define k+ (k-style +))
(define k- (k-style -))
(define k* (k-style *))
(define k/ (k-style /))
(define k= (k-style =))
(define k< (k-style <))
(define k> (k-style >))
(define knot (k-style not))
(define ksqrt (k-style sqrt))


;; --- cps style ---

(define (kpyth x y k)
  (k* x x (lambda (sx)
            (k* y y (lambda (sy)
                      (k+ sx sy (lambda (s)
                          (ksqrt s k))))))))

(define (kfact n k)
  (k= n 0 (lambda (test)
            (if test
                (k 1)
                (k- n 1 (lambda (m)
                          (kfact m (lambda (u)
                                     (k* n u k)))))))))

