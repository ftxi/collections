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

(define-image (whirlpool t c x)
  (draw-sequence
   ((brush) (get-brush c))
   ((polygon) (rp2 -0.5 (- (/ (sqrt 3.0) 6.0)))
              (rp2 0.5 (- (/ (sqrt 3.0) 6.0)))
              (rp2 0.0 (/ (sqrt 3) 3)))
   (whirlpool (let* ((sqrt3/3 (/ (sqrt 3) 3))
                     (a (bAc->a x (/ pi 6) sqrt3/3))
                     (theta (abc->A x a sqrt3/3)))
                (transformation-compose t
                                        (scale (/ a sqrt3/3))
                                        (rotate theta)))
              (inverse-color c)
              x)))

(draw (whirlpool (transformation-compose (scale 500.0) (translate 0.5 (/ (sqrt 3) 3)) (rotate pi)) 'black 1/3) #:size 550)
(draw (whirlpool (transformation-compose (scale 550.0) (translate 0.5 (/ (sqrt 3) 3)) (rotate pi) (scale 3.0)) 'black 1/7) #:size 550)
(send (draw (whirlpool (transformation-compose (scale 4096.0) (translate 0.5 (/ (sqrt 3) 3)) (rotate pi) (scale 0.9)) 'black 1/3) #:size 4096)
      save-file "triangles.png" 'png)
(send (draw (whirlpool (transformation-compose (scale 4096.0) (translate 0.5 (/ (sqrt 3) 3)) (rotate pi) (scale 3.0)) 'black 1/7) #:size 4096)
      save-file "whirlpool.png" 'png)

(define-image (kohler0 t)
  (letrec ((sqrt3 (sqrt 3.0))
           (kohler-triangle
            (lambda-image (t)
              (draw-sequence
               ((polygon) (rp2 -0.5 0.0)
                          (rp2  0.5 0.0)
                          (rp2  0.0 (/ sqrt3 2)))
               (kohler-triangle (transformation-compose t
                                                        (centering -0.5 0.0 (rotate (/ pi 3)))
                                                        (scale (/ 1.0 3.0))))
               (kohler-triangle (transformation-compose t
                                                        (centering  0.5 0.0 (rotate (- (/ pi 3))))
                                                        (scale (/ 1.0 3.0))))))))
    (draw-sequence
     ((brush) (get-brush 'black))
     ((polygon) (rp2 -0.5 (/ sqrt3 6))
                (rp2  0.5 (/ sqrt3 6))
                (rp2  0.0 (- (/ sqrt3 3))))
     (kohler-triangle (transformation-compose t
                                              (translate 0.0 (/ sqrt3 6))
                                              (scale (/ 1.0 3.0))))
     (kohler-triangle (transformation-compose t
                                              (rotate (* 2/3 pi))
                                              (translate 0.0 (/ sqrt3 6))
                                              (scale (/ 1.0 3.0))))
     (kohler-triangle (transformation-compose t
                                              (rotate (* 4/3 pi))
                                              (translate 0.0 (/ sqrt3 6))
                                              (scale (/ 1.0 3.0)))))))

(draw (kohler0 (transformation-compose (scale 400.0) (translate 0.5 (/ (sqrt 3) 3)) (rotate pi) (scale 0.7))) #:size 400)

#|
(define-image (kohler t)
  (letrec ((sqrt3 (sqrt 3.0))
           (kohler-triangle
            (lambda-image (t)
              (draw-sequence
               ((polygon) (rp2 -0.5 0.0)
                          (rp2  0.5 0.0)
                          (rp2  0.0 (/ sqrt3 2)))
               (kohler-triangle (transformation-compose t
                                                        (centering -0.5 0.0 (rotate (/ pi 3)))
                                                        (scale (/ 1.0 3.0))))
               (kohler-triangle (transformation-compose t
                                                        (centering -0.5 0.0 (rotate (/ pi 3)))
                                                        (translate (/ 1.0 3.0) 0.0)
                                                        (scale (/ 1.0 9.0))))
               (kohler-triangle (transformation-compose t
                                                        (centering -0.5 0.0 (rotate (/ pi 3)))
                                                        (translate (- (/ 1.0 3.0)) 0.0)
                                                        (scale (/ 1.0 9.0))))
               (kohler-triangle (transformation-compose t
                                                        (centering  0.5 0.0 (rotate (- (/ pi 3))))
                                                        (scale (/ 1.0 3.0))))
               (kohler-triangle (transformation-compose t
                                                        (centering  0.5 0.0 (rotate (- (/ pi 3))))
                                                        (translate (/ 1.0 3.0) 0.0)
                                                        (scale (/ 1.0 9.0))))
               (kohler-triangle (transformation-compose t
                                                        (centering  0.5 0.0 (rotate (- (/ pi 3))))
                                                        (translate (- (/ 1.0 3.0)) 0.0)
                                                        (scale (/ 1.0 9.0))))
               ))))
    ;(kohler-triangle t)
    (draw-sequence
     ((brush) (get-brush 'black))
     ((polygon) (rp2 -0.5 (/ sqrt3 6))
                (rp2  0.5 (/ sqrt3 6))
                (rp2  0.0 (- (/ sqrt3 3))))
     (kohler-triangle (transformation-compose t
                                              (translate 0.0 (/ sqrt3 6))
                                              (scale (/ 1.0 3.0))))
     (kohler-triangle (transformation-compose t
                                              (translate (/ 1.0 3.0) (/ sqrt3 6))
                                              (scale (/ 1.0 9.0))))
     (kohler-triangle (transformation-compose t
                                              (rotate (* 2/3 pi))
                                              (translate 0.0 (/ sqrt3 6))
                                              (scale (/ 1.0 3.0))))
     (kohler-triangle (transformation-compose t
                                              (rotate (* 4/3 pi))
                                              (translate 0.0 (/ sqrt3 6))
                                              (scale (/ 1.0 3.0)))))))

(draw (kohler (transformation-compose (scale 400.0) (translate 0.5 (/ (sqrt 3) 3)) (rotate pi) (scale 0.7))) #:size 400)
|#