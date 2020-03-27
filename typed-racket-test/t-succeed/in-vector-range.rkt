
#lang typed/racket/base #:transient

;(: v : (Vectorof Integer))
(define v (vector 1 2 3))
(for ([t (in-vector v 0 2)])
    (displayln t))
