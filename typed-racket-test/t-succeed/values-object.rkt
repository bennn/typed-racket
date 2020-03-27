#lang typed/racket #:transient

(define v values)
;(define v identity)

(: f (Boolean -> #f))
(define (f x) (if (v x) #f x))
