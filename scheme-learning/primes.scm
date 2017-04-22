
(define atom?
    (lambda (s)
        (and (not (null? s)) (not (pair? s)))))

(define firsts
    (lambda (s)
        (cond
            ((null? s) (quote ()))
            (#t (cons (car (car s)) (firsts (cdr s)))))))

(define prime?
    (lambda (x)
        ((lambda (s)
            (define loop
                (lambda (n)
                    (cond
                        ((> n s) #t)
                        ((= (remainder x n) 0) #f)
                        (#t (loop (+ 1 n))))))
            (loop 2)) (sqrt x))))

(define factors
    (lambda (x)
        (define sub-factors
            (lambda (u n)
                (cond
                    ((= u 1) (quote ()))
                    ((= (remainder u n) 0) (cons n (sub-factors (/ u n) n)))
                    (#t (sub-factors u (+ n 1))))))
        (sub-factors x 2)))
