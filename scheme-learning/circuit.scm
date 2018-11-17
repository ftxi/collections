
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
  
;; the syntax(ices) for abbrivation

(define-syntax tell
  (syntax-rules ()
    ((tell sth action . input)
     ((sth 'action) . input))))

(define-syntax define-wire
  (syntax-rules ()
    ((define-wire a)
     (define a (make-wire)))))

;; the structure of a wire

(define (make-wire)
  (let ((state unspecific)
        (action-procs '()))
    (letrec
        ((get-state (lambda () state))
         (set-state!
          (lambda (new-state)
            (set! state new-state)
            (call-each action-procs)))
         (add-proc!
          (lambda (proc)
            (set! action-procs (cons proc action-procs)))))
      (lambda (op)
        (cond
          ((eq? op 'get-state) get-state)
          ((eq? op 'set-state!) set-state!)
          ((eq? op 'add-proc!) add-proc!)
          (else (error "wire: command not found:" op)))))))

(define (call-each procs)
  (map (lambda (x) (x)) procs)
  unspecific)

;; circuit operations

(define (and-gate a b o)
  (let ((and-gate-procedure
         (lambda ()
           (let ((va (tell a get-state))
                 (vb (tell b get-state)))
             (tell o set-state! (l/and va vb))))))
    (tell a add-proc! and-gate-procedure)
    (tell b add-proc! and-gate-procedure)))

(define (l/and u v)
  (cond
    ((= u 0) 0)
    ((= v 0) 0)
    (else 1)))
           
(define (or-gate a b o)
  (let ((or-gate-procedure
         (lambda ()
           (let ((va (tell a get-state))
                 (vb (tell b get-state)))
             (tell o set-state! (l/or va vb))))))
    (tell a add-proc! or-gate-procedure)
    (tell b add-proc! or-gate-procedure)))

(define (l/or u v)
  (cond
    ((not (= u 0)) 1)
    ((not (= v 0)) 1)
    (else 0)))

(define (not-gate i o)
  (let ((not-gate-procedure
         (lambda ()
           (let ((v (tell i get-state)))
             (tell o set-state! (l/not v))))))
    (tell i add-proc! not-gate-procedure)))

(define (l/not v)
  (cond
    ((= v 0) 1)
    (else 0)))

(define (probe w name)
  (tell w
        add-proc!
        (lambda ()
          (shows name "is now:" (tell w get-state)))))

;;;;;;;;;;;;;;;;;;;;;;
;;;   test phase   ;;;
;;;;;;;;;;;;;;;;;;;;;;

(define-wire a)
(define-wire b)
(define-wire c)
(define-wire d)
(and-gate a b c)
(not-gate c d)
(probe a 'a)
(probe b 'b)
(probe d 'd)
(tell a set-state! 0)
(tell b set-state! 1)

