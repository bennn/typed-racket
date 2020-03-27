#lang typed/racket/base #:transient

(: f (Any -> Boolean : Number))
(define f (make-predicate Number))

(: g (Listof Number))
(define g (filter f '(1 2 3 4 "5")))
