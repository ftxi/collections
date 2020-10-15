#lang racket

(require racket/stxparam)
(require "transformations.rkt")

(define (text-pict name)
  (lambda s
    (display "pict: ")
    (display name)
    (display " at ")
    (map display s)
    (newline)))

(define text-triangle (text-pict 'triangle))

(define-syntax-parameter rp2 #f) ;; relative-position-2-dimensional
(define-syntax-parameter ap2 #f) ;; absolute-position-2-dimensional
(define-syntax-parameter triangle #f)
(define-syntax define-image
  (syntax-rules ()
    ((define-image (name T args ...) procs ...)
     (define (name action T args ...)
       (let ((make-relative-pos2 (lambda (x y)
                    (apply-transformation T (make-pos2 x y))))
             (size (transformation-size T)))
         (when (> size 0.001)
           (cond
             ((eq? action 'draw)
              (syntax-parameterize ((rp2 (make-rename-transformer #'make-relative-pos2))
                                    (ap2 (make-rename-transformer #'make-pos2))
                                    (triangle (make-rename-transformer #'text-triangle)))
                procs ...)))))))))

;       (cond
;         ((eq? action 'draw)
;          (syntax-parameterize ((pos2 (make-rename-transformer #'make-pos2))
;                                (triangle (make-rename-transformer #'foo)))
;            procs ...)))))))

(define-image (simple t)
  (triangle (rp2 0.0 0.0)
            (rp2 1.0 0.0)
            (rp2 0.5 (/ (sqrt 3) 2))))

(simple 'draw (scale 0.00001))