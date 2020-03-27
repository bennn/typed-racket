#lang racket
(module m typed/racket #:transient
  (provide x)
  (define-type x 'x #:omit-define-syntaxes)
  (define x : x 'x))
(module n typed/racket #:transient
  (require (submod ".." m))
  x ; works fine, outputs 'x
  (define y : x x))
