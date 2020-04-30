#lang racket/load

;;bg NO ERROR

(module a typed/racket #:transient (define v values) (provide v))
(require 'a)
v
