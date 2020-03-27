#lang racket

(module m typed/racket/base #:transient
  (define x 1)
  (provide x))

(module n typed/racket/base #:transient
  (require (submod ".." m))
  (module* a #f x))
