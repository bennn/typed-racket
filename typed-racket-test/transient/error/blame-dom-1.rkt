#lang racket/base

;; Export function,
;; error in mandatory arg,
;; blame 1 boundary

(module t typed/racket/base #:transient
  (: fff (-> Symbol Void))
  (define (fff x)
    (void))
  (provide fff))

(require 't)

(fff 000)
