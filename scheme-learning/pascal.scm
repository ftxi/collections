
(define (pascal-elem depth n)
    (cond
        ((= n 1) 1)
        ((= n depth) 1)
        (else
            (+  (pascal-elem (- depth 1) (- n 1))
                (pascal-elem (- depth 1) n)))))
