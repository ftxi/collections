#lang racket

(define (shows . s)
  (cond ((null? s)
         (newline))
        (else (display (car s))
              (apply shows (cdr s)))))

(define (id x) x)

(define-syntax cond+
  (syntax-rules (else)
    ((cond+) (cond))
    ((cond+ (else x ...))
     (begin x ...))
    ((cond+ (p k x ...) s ...)
     (let ((k p))
       (if k
           (begin x ...)
           (cond+ s ...))))))

;(cond+
; ((= 1 2) p (display p))
; ((+ 2 2) p (display p))
; (else #f))

(define operators-list
  `((* / %)
    (+ -)
    (== != > < >= <=)
    (&& ,(string->symbol "||"))))

(define operator-string-list
  (map (lambda (x) (list (symbol->string x) x))
       (flatten operators-list)))

(define (to-substrings str)
  (string-split
   (let ((brakets (string->list "()[]"))
         (syms (string->list "!@#$%^&*+-=:/~\\<>|")))
     (let loop ((left (string->list "  ")) (right (string->list str)))
       (if (null? right)
           (list->string (reverse left))
           (let ((x (car right)))
             (cond
               ((member x brakets)
                (loop (cons #\space (cons x (cons #\space left))) (cdr right)))
               ((member x syms)
                (if (member (car left) syms)
                    (loop (cons x left) (cdr right))
                    (loop (cons x (cons #\space left)) (cdr right))))
               (else
                (if (member (car left) syms)
                    (loop (cons x (cons #\space left)) (cdr right))
                    (loop (cons x left) (cdr right)))))))))))

(define (handle-words w)
  (cond+
    ((string->number w) n n)
    ((assoc w operator-string-list) pair (cadr pair))
    (else w)))

(define (map* f l)
  (cond
    ((null? l) l)
    ((pair? l)
     (cons (map* f (car l))
           (map* f (cdr l))))
    (else (f l))))

(define (handle-parenthesis ls)
  (car
   (let loop ((left '()) (right ls))
     ;(shows "loop " left " -- " right)
     (cond
       ((null? right)
        ;(shows "case 1")
        (cons (reverse left) '()))
       ((equal? (car right) ")")
        ;(shows "case 2")
        (cons (reverse left) (cdr right)))
       ((equal? (car right) "(")
        ;(shows "case 3")
        (let ((res (loop '() (cdr right))))
          (loop (cons (car res) left) (cdr res))))
       (else
        ;(shows "case 4 -- " (car right) "  --  " (eqv? (car right) "["))
        (loop (cons (car right) left) (cdr right)))))))

;(string-replace s0 "(" " ( ")

(define (split-at-first syms ls)
  (let loop ((left '()) (right ls))
    (if (null? right)
        #f
        (let ((x (car right)))
          ;(shows "loop " left " -- " right)
          (cond
            ((member x syms)
             (list x (reverse left) (cdr right)))
            (else
             (loop (cons x left) (cdr right))))))))

(define rev-operator-list (reverse operators-list))

(define (polish-style ls)
  (let loop ((l ls) (remaining-ops rev-operator-list))
    ;(shows "loop " l " -- " remaining-ops)
    (cond ((or (not (pair? l)) (null? remaining-ops)) l)
          ((and (pair? l) (null? (cdr l))) (loop (car l) remaining-ops))
          (else
           (let* ((current-ops (car remaining-ops))
                  (s (split-at-first current-ops l)))
             ;(shows s " -- " current-ops)
             (if s
                 (let ((p1 (car s))
                       (p2 (loop (cadr s) rev-operator-list))
                       (p3 (loop (caddr s) rev-operator-list)))
                   (if (null? p2)
                       (list p1 p3)
                       (list p1 p2 p3)))
                 (loop l (cdr remaining-ops))))))))

(define (lexical-parse s)
  (polish-style (map* handle-words (handle-parenthesis (to-substrings s)))))

(define s0 " (1+  2* (4    +56)  +3) == sin n")
(to-substrings s0)
(handle-parenthesis (to-substrings s0))
(map* handle-words (handle-parenthesis (to-substrings s0)))
(polish-style (map* handle-words (handle-parenthesis (to-substrings s0))))

(define operators-procedure
  (list (list '+ +) (list '- -) (list '* *) (list '/ /) (list '% remainder)
        (list '== (lambda s (if (apply = s) 1 0)))
        (list '!= (lambda s (if (apply = s) 0 1)))
        (list '>  (lambda s (if (apply > s) 1 0)))
        (list '<  (lambda s (if (apply < s) 1 0)))
        (list '>= (lambda s (if (apply >= s) 1 0)))
        (list '<= (lambda s (if (apply <= s) 1 0)))
        (list '&& (lambda (x y) (if (not (or (= x 0) (= y 0))) 1 0)))
        (list (string->symbol "||") (lambda (x y) (if (not (and (= x 0) (= y 0))) 1 0)))))

(define (calculate m)
  (cond
    ((number? m) m)
    #;((not (pair? m))
     (shows m)
     (error "^unexpected expression"))
    (else
     (let ((operator (cadr (assq (car m) operators-procedure)))
           (operands (map calculate (cdr m))))
       (apply operator operands)))))
    

;(split-at-first '(3 4) '(1 2 3 4 5 6))

#|
(define (parse-exp s)
  (cond+
    ((regexp-match #px"^\\s*\\((.*)\\)\\s*$" s)
     pat
     (parse-exp (cadr pat)))
    (else s)))
|#
