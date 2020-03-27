#lang racket/load

(module untyped racket
  (provide f)
  (define (f g) 'g))
    

(module typed typed/racket #:transient
  (require/typed 'untyped
    [f ((Any -> Boolean : Symbol) -> Symbol)]))

(require 'typed)
