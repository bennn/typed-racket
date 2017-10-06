#lang racket/base

(module t typed/racket/base
  (provide f)

  (define-type Spec
    (-> (U Spec String)))

  (: f (-> Spec))
  (define (f)
    (λ () "hello")))

(require 't)

f
