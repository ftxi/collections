#lang scheme

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-valude
           (logical-and (get-signal a1)
                        (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output
                                  new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a1 and-action-procedure))

(define (make-wire)
  (let ((signal 0) (action-procedures '()))
    (define (set-my-signal! new)
      (cond ((= signal new) 'done)
            (else
             (set! signal new)
             (call-each action-procedures))))
    (define (accept-action-procedure precedure)
      (set! action-procedure
            (cons procedure action-procedures))
      (procedure))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure)
            (else
             (error "Bad message" m))))
    dispatch))