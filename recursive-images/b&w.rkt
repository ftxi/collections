#lang racket

(require racket/draw)
(require "draw.rkt")
(require "transformations.rkt")

(define black-brush (new brush% (color "black")))
(define white-brush (new brush% (color "white")))

(define (get-brush color)
  (cond
    ((eq? color 'black) black-brush)
    ((eq? color 'white) white-brush)))

(define (inverse-color color)
  (cond
    ((eq? color 'black) 'white)
    ((eq? color 'white) 'black)))

;; solving triangles

(define (bAc->a b A c)
  (sqrt (- (+ (* b b)
              (* c c))
           (* 2 b c (cos A)))))

(define (abc->A a b c)
  (acos (/ (- (+ (* b b)
                 (* c c))
              (* a a))
           (* 2 b c))))

;; definition of images

(define-image (simple t c)
  (draw-sequence
   ((brush) (get-brush c))
   ((polygon) (rp2 0.0 0.0)
              (rp2 1.0 0.0)
              (rp2 0.5 (/ (sqrt 3.0) 2.0)))
   (simple (transformation-compose t (scale 0.5))
           (inverse-color c))))

(draw (simple (transformation-compose (scale 180.0) (translate 1.0 1.0) (rotate pi)) 'black))

(define-image (whirlpool t c)
  (draw-sequence
   ((brush) (get-brush c))
   ((polygon) (rp2 -0.5 (- (/ (sqrt 3.0) 6.0)))
              (rp2 0.5 (- (/ (sqrt 3.0) 6.0)))
              (rp2 0.0 (/ (sqrt 3) 3)))
   (whirlpool (let* ((sqrt3/3 (/ (sqrt 3) 3))
                     (a (bAc->a 1/3 (/ pi 6) sqrt3/3))
                     (theta (abc->A 1/3 a sqrt3/3)))
                (transformation-compose t
                                        (scale (/ a sqrt3/3))
                                        (rotate theta)))
              (inverse-color c))))

(draw (whirlpool (transformation-compose (scale 500.0) (translate 0.5 (/ (sqrt 3) 3)) (rotate pi)) 'black) #:size 600)

