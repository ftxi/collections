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
	    (set-cdr! table s))))
       (contents
        (lambda ()
          (cdr table))))
    (lambda (message)
      (cond
        ((eq? 'contents message) contents)
        ((eq? 'lookup message) lookup)
        ((eq? 'insert! message) insert!)
        #|(else (error "Bad message: " message))|#))))

(define (lookup key table)
  ((table 'lookup) key))

(define (insert! key val table)
  ((table 'insert!) key val))

(define (content-of table)
  ((table 'contents)))

(define (memorize f)
  (let ((table (make-table)))
    (lambda (x)
      (or (lookup x table)
          (let ((v (f x)))
            (insert! x v table)
            v)))))

#|

(define-syntax define-method
  (syntax-rules ()
    ((define-method method-name)
     (define method-name
       (lambda arguments
         (((car arguments) (quote method-name)) (cdr arguments)))))
    ((define-method method-name ...)
     (define method-name
       (lambda arguments
         (((car arguments) (quote method-name)) (cdr arguments))))
     (define-method ...))))

|#

;; --------------------------

(define fib
  (memorize
   (lambda (n)
     (cond
       ((< n 2) n)
       (else (+ (fib (- n 1))
                (fib (- n 2))))))))

;; the Catalan numbers, see <http://oeis.org/A000108>
(define tc
  (memorize
   (lambda (n)
     (if (= n 1)
         1
         (let loop ((k 1) (s 0))
           (if (= k n)
               s
               (loop (+ k 1)
                     (+ s (* (tc k) (tc (- n k)))))))))))

