#lang r5rs

(define (make-table)
  (letrec 
      ((table (list '*table*))
       (lookup
	(lambda (key)
	  (let ((v (assoc key (cdr table))))
	    (and v (cadr v)))))
       (insert!
	(lambda (key val)
	  (let ((s (list (list key val))))
	    (set-cdr! s (cdr table))
	    (set-cdr! table s)))))
    (lambda (message)
      (cond
       ((eq? 'lookup message) lookup)
       ((eq? 'insert! message) insert!)
       #|(else (error "Bad message: " message))|#))))

(define (lookup key table)
  ((table 'lookup) key))

(define (insert! key val table)
  ((table 'insert!) key val))

(define (memorize f)
  (let ((table (make-table)))
    (lambda (x)
      (or (lookup x table)
          (let ((v (f x)))
            (insert! x v table)
            v)))))

;; --------------------------

(define fib
  (memorize
   (lambda (n)
     (cond
       ((< n 2) n)
       (else (+ (fib (- n 1))
                (fib (- n 2))))))))
