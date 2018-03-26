#lang racket/gui

(require racket/date)

(define (gauge/make-progress gauge)
  (let ((range (send gauge get-range))
        (value (send gauge get-value)))
    (when (< value range)
      (send gauge set-value (+ 1 value)))))

; Make a frame by instantiating the frame% class
(define frame (new frame%
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


(define gauge (new gauge%
                   [label "Gauge"]
                   [parent frame]
                   [range 100]))

; Derive a new canvas (a drawing window) class to handle events
(define my-canvas%
  (class canvas% ; The base class is canvas%
    ; Define overriding method to handle mouse events
    (define/override (on-event event)
      (send msg set-label (format "Canvas mouse: at (~s, ~s)"
                                  (send event get-x)
                                  (send event get-y))))
    ; Define overriding method to handle keyboard events
    (define/override (on-char event)
      (send msg set-label (format "Canvas keyboard: ~a" (send event get-key-release-code))))
    ; Call the superclass init, passing on all init args
    (super-new)))
 
; Make a canvas that handles events in the frame
(new my-canvas% [parent frame] [min-height 270])

(define time-msg (new message%
                      [parent (new horizontal-pane%
                                   [parent frame]
                                   [stretchable-height #f])]
                      [auto-resize #t]
                      [label "..."]))

(new timer%
     [interval 1000]
     [notify-callback (lambda ()
                        (send time-msg
                              set-label
                              (format "The time is: ~a"
                                      (date->string (current-date) #t))))])

; Make a myutable text message in the frame
(define msg (new message% [parent (new horizontal-pane% [parent frame])]
                          [auto-resize #t]
                          [label "No events so far..."]))
 
; Show the frame by calling its show method
(send frame show #t)


