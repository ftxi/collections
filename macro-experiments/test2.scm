#lang racket

(require math/array
         math/matrix)

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
      (amb)
      #f))

(let* (
       ;(B (matrix+ A (matrix* A A)))
       (C (array #[#[3 0 0 3]
                   #[0 4 2 0]
                   #[0 2 4 0]
                   #[3 0 0 3]]))
       (A (vector->array #(4 4) (for/vector ((x (range 16)))
                                  (if (member x '(1 2 4 7 8 11 13 14))
                                      0
                                      (amb 0 1 -1 2 -2 3)))))
       )
;  (display A)
;  (newline)
;  (display B)
;  (newline)
  (for ((x (in-range 4))
        (y (in-range 4)))
    (assert (= (array-ref C (vector x y))
               (+ (array-ref A (vector x y))
                  (for/sum ((k (in-range 4)))
                    (* (array-ref A (vector x k))
                       (array-ref A (vector k y))))))))
  (assert (matrix= A C))
  (display "\nokay\n")
  (display A)
  )

