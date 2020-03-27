#lang typed/racket #:transient

(define-new-subtype F (make-F (-> Real Real)))

(: app : F Real -> Real)
(define (app f x)
  (f x))

