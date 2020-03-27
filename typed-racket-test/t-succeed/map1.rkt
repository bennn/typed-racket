#lang typed-scheme #:transient

(: f ((U String Number) -> Void))
(define (f x) (void))

(map f (list 1 2 3))
