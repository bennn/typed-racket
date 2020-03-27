#lang typed-scheme #:transient

(define-type-alias Int Integer)

(: foo ( -> Int))
(define (foo)
 (: loop (Int -> Int))
 (define (loop x)
   (loop x))
 (loop 0))
