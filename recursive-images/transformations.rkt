#lang typed/racket

(require math/array
         math/matrix)

(provide make-pos2 pos2-x pos2-y
         identity-transformation
         translate rotate scale diagonal-transformation
         vertical-flip horizontal-flip
         centering axis-rotating
         transformation-compose apply-transformation
         transformation-size)

(define-type transformation (Matrix Flonum))

(define-type position (Matrix Flonum))

(: make-pos2 (Flonum Flonum . -> . position))
(define (make-pos2 x y)
  (col-matrix [x y 1.0] : Flonum))

(: pos2-x (position . -> . Flonum))
(define (pos2-x p)
  (array-ref p #(0 0)))

(: pos2-y (position . -> . Flonum))
(define (pos2-y p)
  (array-ref p #(1 0)))

(: identity-transformation transformation)
(define identity-transformation (identity-matrix 3 1.0 0.0))

(: translate (Flonum Flonum . -> . transformation))
(define (translate tx ty)
  (matrix [[1.0 0.0 tx]
           [0.0 1.0 ty]
           [0.0 0.0 1.0]]))

(: rotate (Flonum . -> . transformation))
(define (rotate theta)
  (let ((c (cos theta))
        (s (sin theta)))
    (matrix [[  c (- s) 0.0]
             [  s    c  0.0]
             [0.0  0.0  1.0]])))

(: diagonal-transformation (Flonum Flonum . -> . transformation))
(define (diagonal-transformation sx sy)
  (matrix [[ sx 0.0 0.0]
           [0.0  sy 0.0]
           [0.0 0.0 1.0]]))

(: scale (Flonum . -> . transformation))
(define (scale s)
  (diagonal-matrix (list s s 1.0) 0.0))

(: vertical-flip transformation)
(define vertical-flip (diagonal-matrix (list 1.0 -1.0 1.0) 0.0))

(: horizontal-flip transformation)
(define horizontal-flip (diagonal-matrix (list -1.0 1.0 1.0) 0.0))

(: centering (Flonum Flonum transformation . -> . transformation))
(define (centering ux uy T)
  (matrix* (translate ux uy) T (translate (- ux) (- uy))))

(: axis-rotating (Flonum transformation . -> . transformation))
(define (axis-rotating theta T)
  (matrix* (rotate theta) T (rotate (- theta))))

(: transformation-compose (transformation transformation * . -> . transformation))
(define transformation-compose matrix*)

(: apply-transformation (transformation position . -> . position))
(define apply-transformation matrix*)

;; use L^2 operator norm for upper bound
(: transformation-size (transformation . -> . Number))
(define (transformation-size T)
  (let ((S (array-slice-ref T '((0 1) (0 1)))))
    (sqrt (matrix-trace (matrix* S (matrix-transpose S))))))
