#lang racket

(letrec-image
 ((tris (λ (color transformation)
          (draw-sequence
           (triangle color
                     (pos 0 0)
                     (pos 1 0)
                     (pos 1/2 (/ (sqrt 3) 2)))
           (tris (inverse-color color)
                 (compose-transformation
                  (translate (vec (3/4 (/ (sqrt 3) 4))))
                  (rotate pi)
                  (scale 1/2)))))))
 (tris black identity-transformation))

