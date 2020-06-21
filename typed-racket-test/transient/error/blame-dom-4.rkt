#lang racket/base

;; Export function,
;; error in optional arg,
;; blame 1 boundary

(module t typed/racket/base #:transient
  (: fff (->* [Symbol] [String Real] Void))
  (define (fff x [y "y"] [r 42])
    (string-append y "yyyy")
    (void))
  (provide fff))

(require 't)

(fff 'sym 'not-str)
