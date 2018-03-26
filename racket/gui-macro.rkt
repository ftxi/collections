#lang racket/gui


(define-syntax make-widget
  (syntax-rules ()
    ((make-widget (type ([k . v] ...)))
     (new type [k . v] ...))))

(define-syntax make-widgets
  (syntax-rules ()
    ((make-widgets (type ([k . v] ...))) ;; the basic form
     (make-widget (type ([k . v] ...))))
    ((make-widgets (type (label-name [k . v] ...))) ;; implied label argument
     (make-widgets (type ([k . v] ... [label label-name])))) 
    ((make-widgets parent-name (type (args ...) things ...)) ;; with the parent's name specified
     (make-widgets (type (args ... [parent parent-name]) things ...)))
    ((make-widgets parent-name (type name (args ...) things ...)) ;; with both the parent's name specified, and a name given
     (make-widgets (type name (args ... [parent parent-name]) things ...)))
    ((make-widgets (type name (args ...) child children ...))
     (begin
       (make-widgets (type name (args ...)))
       (make-widgets "to_settle_the_children" name child children ...)))
    ((make-widgets "to_settle_the_children" parent-name last-child)
     (make-widgets parent-name last-child))
    ((make-widgets "to_settle_the_children" parent-name child children ...)
     (begin
       (make-widgets parent-name child)
       (make-widgets "to_settle_the_children" parent-name children ...)))
    ((make-widgets (type name (args ...) things ...)) ;; with a name given
     (define name (make-widgets (type (args ...) things ...))))))

(make-widgets
 (frame% frame ("Title" [width 400] [height 300])
         (button% button1 ("button" [callback (λ (b e) (send msg set-label "Button click"))]))
         (message% msg ("Nothing so far" [auto-resize #t]))))

(send frame show #t)