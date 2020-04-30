#lang racket

;;bg NO ERROR

(module t typed/racket #:no-optimize #:transient
  (provide f g)

  (define f (ann (case-lambda [() (add1 "hello")] [(x) x]) (Number -> Number)))
  (define g (ann f Any)))

(require 't)
(f 1)
(g)
