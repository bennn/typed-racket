#lang typed/racket/base #:transient

(module u racket/base
  (define x* "not function")
  (provide x*))

(require/typed 'u
  (#:opaque X? x*))

