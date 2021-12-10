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

(define-image (cross t)
  (draw-sequence
   (brush (get-brush 'black))
   ((polygon) (rp2 0.1 0.0)
              (rp2 1.0 0.9)
              (rp2 0.9 1.0)
              (rp2 0.0 0.1)
              (rp2 -0.9 1.0)
              (rp2 -1.0 0.9)
              (rp2 -0.1 0.0)
              (rp2 -1.0 -0.9)
              (rp2 -0.9 -1.0)
              (rp2 0.0 -0.1)
              (rp2 0.9 -1.0)
              (rp2 1.0 -0.9))))

(draw (cross (transformation-compose (scale 80.0) (translate 1.0 1.0) (rotate pi))))

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
#;(send (draw (whirlpool (transformation-compose (scale 4096.0) (translate 0.5 (/ (sqrt 3) 3)) (rotate pi) (scale 0.9)) 'black 1/3) #:size 4096)
      save-file "triangles.png" 'png)
#;(send (draw (whirlpool (transformation-compose (scale 4096.0) (translate 0.5 (/ (sqrt 3) 3)) (rotate pi) (scale 3.0)) 'black 1/7) #:size 4096)
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

;(draw (kohler0 (transformation-compose (scale 400.0) (translate 0.5 (/ (sqrt 3) 3)) (rotate pi) (scale 0.7))) #:size 400)

(define-image (kohler t)
  (letrec ((sqrt3 (sqrt 3.0))
           (1/3f (/ 1.0 3.0))
           (kohler-line
            (lambda-image (t)
              (draw-sequence
               (kohler-triangle (transformation-compose t (scale 1/3f)))
               (kohler-line (transformation-compose t (translate 1/3f 0.0) (scale 1/3f)))
               (kohler-line (transformation-compose t (translate (- 1/3f) 0.0) (scale 1/3f))))))
           (kohler-triangle
            (lambda-image (t)
              (draw-sequence
               ((polygon) (rp2 -0.5 0.0)
                          (rp2  0.5 0.0)
                          (rp2  0.0 (/ sqrt3 2)))
               (kohler-line (transformation-compose t (centering -0.5 0.0 (rotate (/ pi 3)))))
               (kohler-line (transformation-compose t (centering  0.5 0.0 (rotate (- (/ pi 3))))))))))
    (draw-sequence
     ((brush) (get-brush 'black))
     ((polygon) (rp2 -0.5 (/ sqrt3 6))
                (rp2  0.5 (/ sqrt3 6))
                (rp2  0.0 (- (/ sqrt3 3))))
     (kohler-line (transformation-compose t
                                          (translate 0.0 (/ sqrt3 6))))
     (kohler-line (transformation-compose t
                                          (rotate (* 2/3 pi))
                                          (translate 0.0 (/ sqrt3 6))))
     (kohler-line (transformation-compose t
                                          (rotate (* 4/3 pi))
                                          (translate 0.0 (/ sqrt3 6)))))))

#;(send (draw (kohler (transformation-compose (scale 4096.0) (translate 0.5 (/ (sqrt 3) 3)) (rotate pi) (scale 0.7))) #:size 4096)
      save-file "kohler-snowflake.png" 'png)

#;(text-draw (kohler ;(transformation-compose (scale 400.0) (translate 0.5 (/ (sqrt 3) 3)) (rotate pi) (scale 0.7)) 
                   identity-transformation
                   ) #:size 400)

(define-image (fib345 t)
  (letrec ((theta (+ (/ pi 2) (acos 3/5)))
           (phi (- 0 (/ pi 2) (asin 3/5)))
           (t1 (transformation-compose (centering -0.5 0.5
                                                  (transformation-compose (rotate theta)
                                                                          (scale 0.6)))
                                       (rotate (* -1/2 pi))))
           (t2 (transformation-compose (centering 0.5 0.5
                                                  (transformation-compose (rotate phi)
                                                                          (scale 0.8)))
                                       (rotate (* 1/2 pi))))
           (fib-rectangle
            (lambda-image (t)
              (draw-sequence
               ((brush) (get-brush 'black))
               ((polygon) (rp2 0.5 0.5) (rp2 -0.5 0.5) (rp2 -0.5 -0.5) (rp2 0.5 -0.5))
               (fib-triangle t))))
           (fib-triangle
            (lambda-image (t)
              (draw-sequence
               ((brush) (get-brush 'white))
               ((polygon) (rp2 0.5 0.5) (rp2 -0.5 0.5) (rp2 -0.14 0.98))
               (fib-rectangle (transformation-compose t t1))
               (fib-rectangle (transformation-compose t t2))))))
    (draw-sequence
     ((brush) (get-brush 'black))
     (fib-rectangle t))))

#;(draw (fib345 (transformation-compose (scale 200.0) (translate 0.4 0.75) vertical-flip (scale 0.15))) #:size 200 #:minimal 1.0)

#;(send (draw (fib345 (transformation-compose (scale 1024.0) (translate 0.4 0.75) vertical-flip (scale 0.15))) #:size 1024 #:minimal 0.5)
      save-file "fib345.png" 'png)

(define-image (cactus t)
  (let* ((a 0.25)
         (b 0.22)
         (ha 0.30)
         (ha0 0.25)
         (hb 0.35)
         (hb0 0.30)
         (la (* 0.7 a))
         (lb (* 0.7 b))
         (ma (+ la (* 0.05 a)))
         (mb (+ lb (* 0.05 b)))
         (ta (transformation-compose (translate (- la) ha)
                                     (diagonal-transformation a (* 1.5 a))))
         (tb (transformation-compose (translate lb hb)
                                     (diagonal-transformation b (* 1.8 b)))))
    (letrec ((cactus-branch
              (lambda-image (t)
                (draw-sequence
                 ((polygon) (rp2 -0.05 0.0) (rp2 0.05 0.0) (rp2 0.0 0.81))
                 ((polygon) (rp2 0.0 ha0) (rp2 0.0 ha) (rp2 (- ma) ha))
                 ((polygon) (rp2 0.0 hb0) (rp2 0.0 hb) (rp2 mb hb))
                 (cactus-branch (transformation-compose t ta))
                 (cactus-branch (transformation-compose t tb))))))
      (draw-sequence
       ((brush) (get-brush 'black))
       (cactus-branch t)))))


(draw (cactus (transformation-compose (scale 400.0) (translate 0.5 0.0) (centering 0.0 0.5 vertical-flip))) #:size 400)


(define-image (sweater1 t c)
  (let ((d (inverse-color c)))
    (draw-sequence
     ((polygon) (rp2 0.0 0.0) (rp2 1.0 0.0) (rp2 1.0 1.0) (rp2 0.0 1.0))
     ((brush) (get-brush d))
     ((polygon) (rp2 0.0 0.0) (rp2 0.5 0.5) (rp2 1.0 0.5))
     (sweater (transformation-compose t (scale 0.5)) d)
     (sweater (transformation-compose t (scale 0.5) (translate 0.5 0.0)) d))))

(define-image (sweater2 t c)
  (let ((d (inverse-color c)))
    (draw-sequence
     ((polygon) (rp2 0.0 0.0) (rp2 0.5 0.5) (rp2 1.0 0.0) (rp2 1.0 1.0) (rp2 0.0 1.0))
     ((brush) (get-brush d))
     ((polygon) (rp2 0.0 0.0) (rp2 0.5 0.5) (rp2 1.0 0.5))
     (sweater (transformation-compose t (scale 0.5) (translate 0.0 1.0)) d)
     (sweater (transformation-compose t (scale 0.5) (translate 0.5 1.0)) d))))

(define-image (sweater t c)
  (let ((d (inverse-color c)))
    (draw-sequence
     ((brush) (get-brush c))
     ((polygon) (rp2 0.0 0.0) (rp2 0.5 0.5) (rp2 1.0 0.0) (rp2 1.0 1.0) (rp2 0.0 1.0))
     ((brush) (get-brush d))
     ((polygon) (rp2 0.0 0.0) (rp2 0.5 0.5) (rp2 1.0 0.0))
     (sweater (transformation-compose t (translate -0.25 0.5) (scale 0.5)) d)
     (sweater (transformation-compose t (translate 0.25 0.5) (scale 0.5)) d)
     ;(sweater (transformation-compose t (translate 0.5 0.75) (scale 0.5)) d)
     )))


(draw (draw-sequence
       ((pen) (new pen% (style 'transparent)))
       (sweater (transformation-compose (scale 200.0) (scale 2.0) (translate 0.0 -0.5)) 'white))
      #:size 200)

(send (draw (draw-sequence
       ((pen) (new pen% (style 'transparent)))
       (sweater (transformation-compose (scale 4096.0) (scale 2.0) (translate 0.0 -0.5)) 'black))
      #:size 4096
      ;#:minimal 1.0
      )
      save-file "sweater2.jpg" 'jpeg)
