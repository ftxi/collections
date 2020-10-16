#lang typed/racket

(require math/array
         math/matrix)

(provide make-pos2 pos2-x pos2-y
         identity-transformation
         translate rotate scale
         transformation-compose apply-transformation
         transformation-size)

(define-type transformation (Matrix Flonum))

(define-type position (Matrix Flonum))

(: pos2-x (position . -> . Flonum))
(define (pos2-x p)
  (array-ref p #(0 0)))

(: pos2-y (position . -> . Flonum))
(define (pos2-y p)
  (array-ref p #(1 0)))

(: identity-transformation transformation)
(define identity-transformation (identity-matrix 3 1.0 0.0))

(: make-pos2 (Flonum Flonum . -> . position))
(define (make-pos2 x y)
  (col-matrix [x y 1.0] : Flonum))

(: translate (Flonum Flonum . -> . transformation))
(define (translate tx ty)
  (matrix [[1.0 0.0 tx]
           [0.0 1.0 ty]
           [0.0 0.0 1.0]]))

(: rotate (Flonum . -> . transformation))
(define (rotate theta)
  (let ((c (cos theta))
        (s (sin theta)))
    (matrix [[   c   s 0.0]
             [(- s)  c 0.0]
             [ 0.0 0.0 1.0]])))

(: scale (Flonum . -> . transformation))
(define (scale s)
  (diagonal-matrix (list s s 0.0) 0.0))

(: transformation-compose (transformation transformation * . -> . transformation))
(define transformation-compose matrix*)

(: apply-transformation (transformation position . -> . position))
(define apply-transformation matrix*)

;; use L^2 operator norm for upper bound
(: transformation-size (transformation . -> . Number))
(define (transformation-size T)
  (let ((S (array-slice-ref T '((0 1) (0 1)))))
    (sqrt (matrix-trace (matrix* S (matrix-transpose S))))))

#|
(define (vec3 x y z)
  `#(,x ,y ,z))

(define (vec4 x y z w)
  `#(,x ,y ,z ,w))

(define (vec-x v)
  (vector-ref v 0))

(define (vec-y v)
  (vector-ref v 1))

(define (vec-z v)
  (vector-ref v 2))

(define (vec-w v)
  (vector-ref v 3))

(define (cross
|#