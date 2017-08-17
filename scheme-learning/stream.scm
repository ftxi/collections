
(define-syntax lazy-cons
  (syntax-rules ()
    ((lazy-cons a b)
     (cons a (delay b)))))

(define (lazy-car s)
  (car s))

(define (lazy-cdr s)
  (force (cdr s)))

(define (integer-from x)
  (lazy-cons x (integer-from (+ 1 x))))

(define (lazy-map proc stream)
  (if (null? stream)
      lnil
      (lazy-cons (proc (lazy-car stream))
	     (lazy-cdr stream))))

(define (lazy-filter proc stream)
  (if (null? stream)
      lnil
      (if (proc (lazy-car stream))
	  (lazy-cons (lazy-car stream)
                     (lazy-filter proc (lazy-cdr stream)))
	  (lazy-filter proc (lazy-cdr stream)))))

(define (lazy-accumulate proc start stream)
  (if (null? stream)
      start
      (proc (lazy-car stream)
	    (lazy-accumulate proc start (lazy-cdr stream)))))

(define (lazy-ref n s)
  (if (= n 0)
      (lazy-car s)
      (lazy-ref (- n 1)
		  (lazy-cdr s))))

(define (print-stream s)
  (cond
    ((null? s) '*done*)
    (else (display (lazy-car s))
          (display #\tab)
          (print-stream (lazy-cdr s)))))

(define (enumerate-interval a b)
  (if (> a b)
      '()
      (lazy-cons a (enumerate-interval (+ a 1) b))))

(define (ormap proc l)
  (if (null? l)
      #f
      (or (proc (car l))
          (ormap proc (cdr l)))))

(define (stream-map proc . argstreams)
  (if (ormap null? argstreams)
      '()
      (lazy-cons (apply proc (map lazy-car argstreams))
                 (apply stream-map
                        (cons proc
                              (map lazy-cdr argstreams))))))

(define ones (lazy-cons 1 ones))

(define (add-streams a b)
  (stream-map + a b))

(define integers
  (lazy-cons 1 (add-streams ones integers)))

(define (fib-iter a b)
  (lazy-cons (+ a b)
             (fib-iter b (+ a b))))

(define fibs (fib-iter 0 1))

(define (sieve stream)
  (lazy-cons
   (lazy-car stream)
   (sieve
    (lazy-filter
     (lambda (x)
       (not (= (remainder x (lazy-car stream)) 0)))
     (lazy-cdr stream)))))

(define primes
  (sieve (integer-from 2)))

(define (print-head s n)
  (cond
    ((or (null? s) (= n 0))
     '*done*)
    (else
     (display (lazy-car s))
     (newline)
     (print-head (lazy-cdr s) (- n 1)))))

(define (take n stream)
  (if (= n 0)
      '()
      (cons (lazy-car stream)
            (take (- n 1)
                  (lazy-cdr stream)))))

(define (stream->list stream)
  (lazy-accumulate cons '() stream))
