#lang racket/base

(module a typed/racket #:transient
  (require/typed racket/base [values (-> String String)])
  (provide values))

(module b typed/racket #:transient
  (require/typed (submod ".." a) [values (-> String Any)])
  values)

(require 'b)
