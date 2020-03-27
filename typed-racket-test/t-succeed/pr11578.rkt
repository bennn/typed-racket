#lang racket/load

(module a typed/racket/base #:transient
  (provide foo)
    (struct: foo ()))

(module b racket/base
  (require 'a)
    (foo))


(require 'b)
