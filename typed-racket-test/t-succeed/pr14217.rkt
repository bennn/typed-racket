#lang typed/racket #:transient
(: foo (-> Integer AnyValues))
(define (foo x)
  x)
(foo 5)
