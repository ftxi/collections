
;; basic tools

(define unspecific (cond (#f #f)))

(define (compose f . gs)
  (if (null? gs)
      f
      (lambda (x)
        (f ((apply compose (car gs) (cdr gs)) x)))))

(define (shows . message)
  (let ((display-atom
         (lambda (a)
           (display a)
           (display #\space))))
    (map display-atom message)
    (newline)))

(define (error . s)
  (apply shows (cons "ERROR:" s)))

(define (square x)
  (* x x))

;; find base^exp modulo m
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

;; bzout: take two integers a b and return the Bzout's coefficients m n
;;  -- such that gcd(a,b) = a*m - b*n

(define (bzout a b)
  (letrec ((ecs
            (lambda (a b)
              (if (= b 0)
                  '()
                  (cons (floor (/ a b)) (ecs b (remainder a b))))))
           (collect
            (lambda (u v p q l)
              (if (null? l)
                  (list v q)
                  (collect v (- u (* (car l) v)) q (- p (* (car l) q)) (cdr l))))))
    (collect 1 0 0 1 ((compose reverse cdr reverse) (ecs a b)))))

;; find a number k such that a*k ~ 1 (mod b)
(define (revmod a b)
  (if (not (= (gcd a b) 1))
      (error "revmod:" a "and" b "do not coprime")
      (let ((r (remainder (car (bzout a b)) b)))
        (if (< r 0)
            (+ r b)
            r))))

;; solve the coungrant equation x^p ~ a (mod m)
;; take 4 integers p m and phi, where phi is eularphi(m)
;; this works only when gcd(p,phi) = 1
(define (rootmod p a m phi)
  (let ((r (revmod p phi)))
    (expmod a r m)))

