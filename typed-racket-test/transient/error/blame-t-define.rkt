#lang racket/base

;; Function crosses two boundaries,
;;  only one should be blamed.

(module t typed/racket #:transient
  (require "blame/type-env-t.rkt")

  (: g (-> RR RR))
  (define (g x) x)

  (: h (-> (-> R S) S))
  (define (h y) (y 21))

  (provide g h))

(require 't)

(define cnst42
  (lambda (n) 42))

(void (g cnst42))
(h cnst42)



