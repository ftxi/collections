#lang racket/gui

(define (box-ref box m n)
  (list-ref (list-ref box m) n))

(define (get-u-gen box)
  (lambda (x n)
    (let* ((s (inexact->exact (floor x))) (r (- x s)))
      (+  (cond
            ((= (box-ref box s n) 1) (sin (* r pi)))
            (else 0))
          (cond
            ((< s 1) 0)
            ((= (box-ref box (- s 1) n) 1)
             (sin (+ pi (* r pi))))
            (else 0))))))

(let* ((width 20)
       (height 10)
       (fps 50)
       (refresh-frequency (round (/ 1000 fps)))
       (size 28)
       (size/2 (round (/ size 2)))
       (size/4 (round (/ size 4))))
  (letrec
      ((generate-boxes
        (lambda (n)
          (if (= n 0)
              '()
              (cons (let ((r (random 10)))
                      (cond
                        ((< r 2) '*)
                        (else ':)))
                    (generate-boxes (- n 1))))))
       (stream-cdr
        (lambda (s)
          (force (cdr s))))
       (box-stream
        (cons (generate-boxes height)
              (delay box-stream)))
       (frame (new (class frame%
                     (super-new)
                     (define/augment (on-close)
                       (send timer stop)))
                   [label "Example"]
                   [width 400]
                   [height 300]))
       (timer
        (new timer%
             [interval refresh-frequency]
             [notify-callback (lambda ()
                                (send canvas refresh))]))
       (canvas
        (new (class canvas%
               ; Define overriding method to handle mouse events
               (define/override (on-event event)
                 (send msg set-label (format "Canvas mouse: at (~s, ~s)"
                                             (send event get-x)
                                             (send event get-y))))
               ; Define overriding method to handle keyboard events
               (define/override (on-char event)
                 (send msg set-label (format "Canvas keyboard: ~a"
                                             (send event get-key-release-code))))
               (super-new))
             [parent frame]
             [min-height 270]
             [paint-callback
              (lambda (canvas dc)
                (let* ((t (/ (current-inexact-milliseconds) 200))
                       (u (cos t)))
                  (send* dc
                    (set-brush (if (> (cos (/ t 2)) 0) "red" "gray") 'solid)
                    (set-pen "" 0 'transparent)
                    (draw-rectangle (round (- (- 40 size/2) (* size/4 u)))
                                    40
                                    (round (+ size/2 (* size/2 u)))
                                    size))))]))
       (msg (new message%
                 [parent (new horizontal-pane% [parent frame])]
                 ; Align it to the left side (instead of in the middle)
                 [auto-resize #t]
                 [label "No events so far..."]))
       
       )
    (send frame show #t)
    (displayln box-stream)
    (displayln (stream-cdr box-stream))
    (displayln (stream-cdr (stream-cdr box-stream)))))
