#lang typed/racket/base #:transient

(module u racket/base
  (define x* "OOPS")
  (provide x*))

(require/typed 'u
  (x* (Listof Integer)))

