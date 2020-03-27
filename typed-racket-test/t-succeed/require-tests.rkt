#lang scheme/load
#reader typed-racket/typed-reader
(module bang-tests typed-scheme #:transient
  (define #{x : Number} 1)
  (provide x)
  )

(module trequire typed-scheme #:transient
  (require 'bang-tests)
  (define: y : Number x)
  (display y))
