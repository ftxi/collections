
(define call/cc call-with-current-continuation)
(define fail (lambda s (write "warning: all choices failed") (newline)))

(define-syntax amb
  (syntax-rules ()
    ((amb) (fail))
    ((amb choice) choice)
    ((amb choice rest ...)
     (let ((fail0 fail))
       (call/cc (lambda (k0)
                  (call/cc (lambda (cc)
                             (set! fail cc)
                             (k0 choice)))
                  (set! fail fail0)
                  (amb rest ...)))))))

(define (assert predicate)
  (if (not predicate)
      (amb)))

