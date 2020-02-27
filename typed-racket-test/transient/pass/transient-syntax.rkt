#lang racket/base

(module a typed/racket/base #:transient
  (provide f)
  (define-syntax-rule (f x)
    (car (car x))))

(require 'a)
(f '((a) b c))
