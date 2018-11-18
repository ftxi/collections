
;; A sample program to illustrate the rsa encryptosystem

(load "fast-primes.scm")
(load "number-basic.scm")

(define (string->integers s)
  (map char->integer (string->list s)))

(define (integers->string l)
  (list->string (map integer->char l)))

;; take two prime numbers p q and a number k that is coprime to (p-1)*(q-1)
(define (make-rsa-keys p q k)
  (let* ((m (* p q))
         (phi (* (- p 1) (- q 1)))
         (r (revmod k phi)))
    (lambda (message)
      (cond
        ((eq? message 'get-public-key)
         (list m k))
        ((eq? message 'get-private-key)
         (list m r))
        (else (error "make-rsa-keys: Request not found:" message))))))

(define (get-public-key rsa-keys)
  (rsa-keys 'get-public-key))

(define (get-private-key rsa-keys)
  (rsa-keys 'get-private-key))

(define (crypt l key)
  (let ((m (car key)) (k (cadr key)))
    (map (lambda (x) (expmod x k m)) l)))

(define (encode s key)
  (crypt (string->integers s) key))

(define (decode l key)
  (integers->string (crypt l key)))

;; test phase

(define sample-keys (make-rsa-keys 1997 1999 577))
(define pub-key (get-public-key sample-keys))
(define priv-key (get-private-key sample-keys))

(define code (encode "A quick brown fox jumps over a lazy dog." pub-key))
(shows code)
(shows (decode code priv-key))
