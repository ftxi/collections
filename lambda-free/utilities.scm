
;; utilties.scm
;; to put some tools here, so that I don't have to define them elsewhere

;; the atom predicate, I don't understand why this is not given by default

(define (atom? p)
  (and (not (pair? p))
       (not (null? p))))

;; the undefined value

(define unspecific (if #f 'not-reached))

;; the shows utility, used for debugging

(define (shows . s)
  (cond ((null? s) (newline))
        (else (display (car s))
              (display #\tab)
              (apply shows (cdr s)))))

;; function compose tool

(define (compose f g . s)
  (if (null? s)
      (lambda (x) (f (g x)))
      (lambda (x)
        (f ((apply compose (cons g s)) x)))))

;; general list tools

(define (take n ls)
  (if (= n 0)
      '()
      (cons (car ls)
            (take (- n 1) (cdr ls)))))

(define (drop n ls)
  (if (= n 0)
      ls
      (drop (- n 1) (cdr ls))))

;; the very simplified version of error function

(define error shows)

;; the standard map/filter/reduce operations

;; assuming (list? l) ~> #t

(define (filter p l)
  (cond
    ((null? l) l)
    ((p (car l)) (cons (car l) (filter p (cdr l))))
    (else (filter p (cdr l)))))

;; assuming (list? l) ~> #t

(define (foldr f z l)
  (if (null? l)
      z
      (f (car l) (foldr f z (cdr l)))))

