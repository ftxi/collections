#lang scheme

(define (make-module typename . procs)
  ...)

(define complex-module
  (make-module 'complex
   (proc 'real-part
         (lambda (z)
           (car z)))
   (proc 'imag-part
         (lambda (z)
           (cdr z)))
   (init 'make-complex
         (lambda (r c)
           (cons r c)))
   (proc ':+
         (lambda (u v)
           (make-complex
            (:+ (real-part u) (real-part v))
            (:+ (imag-part u) (imag-part v)))))
   (proc ':-
         (lambda (u v)
           (make-complex
            (:- (real-part u) (real-part v))
            (:- (imag-part u) (imag-part v)))))
   (proc ':*
         (lambda (u v)
           (make-complex
            (:- (:* (real-part u) (real-part v))
                (:* (imag-part u) (imag-part v)))
            (:+ (:* (real-part u) (imag-part v))
                (:* (imag-part u) (real-part v))))))))
            
