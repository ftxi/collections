#lang racket

(require racket/stxparam)
(require "transformations.rkt")

(provide draw-sequence draw-sequence?
         minimal-size
         rp2
         lambda-image define-image
         text-pict text-draw)

;; main part

(define-syntax draw-sequence
  (syntax-rules ()
    ((draw-sequence s ...)
     (delay (list s ...)))))

(define draw-sequence? promise?)

(define minimal-size (make-parameter 0.001))

(define-syntax-parameter rp2 #f) ;; relative-position-2-dimensional
(define-syntax lambda-image
  (syntax-rules ()
    ((lambda-rules (T r ...) procs ...)
     (lambda (T r ...)
       (let ((make-relative-pos2 (lambda (x y)
                                   (apply-transformation T (make-pos2 x y))))
             (size (transformation-size T)))
         (unless (< size (minimal-size))
           (syntax-parameterize ((rp2 (make-rename-transformer #'make-relative-pos2)))
             procs ...)))))))

(define-syntax define-image
  (syntax-rules ()
    ((define-image (name T r ...) procs ...)
     (define name (lambda-image (T r ...) procs ...)))))

;; dummy draw function

(require math/array math/matrix)

(define (text-pict name)
  (lambda s
    (display "pict: ")
    (display name)
    (display " at ")
    (map (lambda (u)
           (cond
             ((and (array? u) (col-matrix? u))
              (display (string-append " (" (~r (pos2-x u) #:precision '(= 4))
                                      ", " (~r (pos2-y u) #:precision '(= 4)) ") ")))
             (else (display u))))
           s)
    (newline)))

(define triangle (make-parameter #f))
(define ap2 (make-parameter #f))

(define (text-draw im)
  (parameterize ((triangle (text-pict 'triangle))
                 (ap2 make-pos2))
    (let rec ((l (force im)))
      (for ((s l))
        (when (draw-sequence? s)
          (rec (force s)))))))

;; test sample
#|

(define-image (simple t)
  (draw-sequence
   ((triangle) (rp2 0.0 0.0)
               (rp2 1.0 0.0)
               (rp2 0.5 (/ (sqrt 3.0) 2.0)))
   (simple (transformation-compose (scale 0.5) t))))

(text-draw (simple identity-transformation))
|#
