#lang typed-scheme #:transient

(: f (All (a) ((Integer a * -> Integer) -> Integer)))
(define (f g) 0)

(f +)
