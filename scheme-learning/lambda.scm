;;; lambda.scm : perform lambda-calculus within scheme

(define (compose f g . s)
  (if (null? s)
      (lambda (x) (f (g x)))
      (lambda (x)
        (f ((apply compose (cons g s)) x)))))

(define (take n ls)
  (if (= n 0)
      '()
      (cons (car ls)
            (take (- n 1) (cdr ls)))))

(define (distinguishable-name names)
  (let ((chars (map (compose string->list
                             symbol->string)
                    names)))
    (let loop ((cs chars) (ans '()))
      (if (null? cs)
          ans
          (loop (cdr cs)
                (let ((m (length (car cs)))
                      (n (length ans)))
                  (cond
                    ((< m n) (loop (cdr cs) ans))
                    ((= m n)
                     (if (equal? (car cs) ans)
                         (append ans (list #\1))
                         ans))
                    (else
                     (if (equal? (take n (car cs)) ans)
                         (append ans
                                 (list (different-charactor (list-ref n (car cs)))))
                         ans)))))))))

(define different-character (compose integer->char (lambda (x) (+ x 1)) char->integer))