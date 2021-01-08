#lang racket

(require racket/draw)
(require "transformations.rkt")
(require "image.rkt")

(provide lambda-image define-image draw-sequence rp2 polygon brush draw text-draw)

(define polygon (make-parameter (text-pict 'polygon)))
(define brush (make-parameter (text-pict 'brush)))

(define (draw im #:size [size 200] #:backing-scale [bs 2.0] #:minimal [ms 0.2])
  (let* ((target (make-bitmap size size #:backing-scale bs))
         (dc (new bitmap-dc% [bitmap target])))
    (parameterize ((minimal-size ms)
                   (polygon (lambda points
                              (send dc draw-polygon
                                    (map (lambda (pt)
                                           (cons (pos2-x pt)
                                                 (pos2-y pt)))
                                         points))))
                   (brush (lambda (b)
                            (send dc set-brush b))))
      (let rec ((l (force im)))
        (for ((s l))
          (when (draw-sequence? s)
            (rec (force s))))))
    target))

