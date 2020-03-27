#lang typed-scheme #:transient

(: x : (Listof (Vectorof Integer)))
(define x (list (vector 1 2 3)))

(list-ref x 0)
