;; This is a sample program of racket user interface in my use

#lang racket/gui

(require racket/date)

(define (gauge/make-progress gauge)
  (let ((range (send gauge get-range))
        (value (send gauge get-value)))
    (when (< value range)
      (send gauge set-value (+ 1 value)))))

; Make a frame by instantiating the frame% class
(define frame (new (class frame%
                     (super-new)
                     (define/augment (on-close)
                       (send timer stop)))
                   [label "Example"]
                   [width 400]
                   [height 300]))
 
; Make a button in the frame
(new button% [parent frame]
     [label "Click Me"]
     ; Callback procedure for a button click:
     [callback (lambda (button event)
                 (send msg set-label "Button click")
                 (gauge/make-progress gauge))])


; Make a gauge
(define gauge (new gauge%
                   [label "Gauge"]
                   [parent frame]
                   [range 100]))
 
; Make a canvas that handles events in the frame
(define canvas
  (new (class canvas% ; The base class is canvas%
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
              (draw-rectangle (round (- 40 (* 10 u))) 40
                              (round (+ 20 (* 20 u))) 40))))]))

(define time-msg (new message%
                      [parent (new horizontal-pane% ; Align it to the left side
                                   [parent frame]
                                   [stretchable-height #f])]
                      [auto-resize #t]
                      [label "..."]))

(define timer
  (new timer%
       [interval 10] ; Update every 10 miliseconds 
       [notify-callback (lambda ()
                          ; Refresh the canvas to draw animation
                          (send canvas refresh)
                          ; Update the time information
                          (send time-msg
                                set-label
                                (format "The time is: ~a"
                                        (date->string (current-date) #t))))]))

; Make a myutable text message in the frame
(define msg (new message%
                 [parent (new horizontal-pane% [parent frame])]
                 ; Align it to the left side (instead of in the middle)
                 [auto-resize #t]
                 [label "No events so far..."]))

; Show the frame by calling its show method
(send frame show #t)
