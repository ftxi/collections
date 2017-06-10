
(define zero (quote zero))
(define one (quote one))
(define two (quote two))
(define three (quote three))
(define four (quote four))
(define five (quote five))
(define six (quote six))
(define seven (quote seven))
(define eight (quote eight))
(define nine (quote nine))

(define decimal-overflow-ceil (quote decimal-overflow-ceil))
(define decimal-overflow-floor (quote decimal-overflow-floor))


(define atom?
    (lambda (s)
        (and (not (null? s)) (not (pair? s)))))

(define zero?
    (lambda (s)
        (and (null? (cdr s) (eq? (car s) zero)))))

(define replace
    (lambda (s t lat)
        (cond
            ((null? lat) (quote ())
            ((eq? s (car lat))
                (cons t (replace (cdr lat))))
            (else (cons (car lat) (replace (cdr lat))))))))

(define inc
    (lambda (s)
        ((lambda (x)
            (cond
                ((and (eq? (car x) decimal-overflow-ceil))
                    (cons one (cons zero (cdr x))))
                (else x))) (sub-inc s))))

(define sub-inc
    (lambda (s)
        (cond
            ((null? s) (quote ()))
            ((atom? s)
                (cond
                    ((eq? s zero) one)
                    ((eq? s one) two)
                    ((eq? s two) three)
                    ((eq? s three) four)
                    ((eq? s four) five)
                    ((eq? s five) six)
                    ((eq? s six) seven)
                    ((eq? s seven) eight)
                    ((eq? s eight) nine)
                    ((eq? s nine) decimal-overflow-ceil)))
            (else ((lambda (t)
                    (cond
                        ((null? t) (cons (sub-inc (car s)) t))
                        ((eq? (car t) decimal-overflow-ceil)
                            (cons (sub-inc (car s)) (cons zero (cdr t))))
                        (else
                            (cons (car s) t)))) (sub-inc (cdr s)))))))

(define dec
    (lambda (s)
        (cond
            ((not (zero? s)) (sub-dec s)))))

(define sub-dec
    (lambda (s)
        (cond
            ((null? s) (quote ()))
            ((atom? s)
                (cond
                    ((eq? s zero) decimal-overflow-floor)
                    ((eq? s one) zero)
                    ((eq? s two) one)
                    ((eq? s three) two)
                    ((eq? s four) three)
                    ((eq? s five) four)
                    ((eq? s six) five)
                    ((eq? s seven) six)
                    ((eq? s eight) seven)
                    ((eq? s nine) eight)))
            (else ((lambda (t)
                    (cond
                        ((null? t) (cons (sub-dec (car s)) t))
                        ((eq? (car t) decimal-overflow-floor)
                            (cons (sub-dec (car s)) t))
                        (else
                            (cons (car s) t)))) (sub-dec (cdr s)))))))

(define add
    (lambda (a b)
        (cond
            ((zero? b) a)
            (else (add (inc a) (dec b))))))
