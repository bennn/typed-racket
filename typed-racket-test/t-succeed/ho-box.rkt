#lang typed/racket #:transient

(: f (Boxof (Number -> Number)))
(define f (box (lambda: ([x : Number]) x)))

(provide f)
