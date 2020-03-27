#lang typed/racket/base #:transient

(: f : Real → Number)
(define (f x)
  (+ 1 (expt 1.0 x)))
(f 0)
