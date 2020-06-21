#lang racket/base

;; Export function,
;; error in mandatory arg,
;; blame 1 boundary

(module t typed/racket/base #:transient
  (: fff (-> Symbol String Real * Void))
  (define (fff x y . r*)
    (void))
  (provide fff))

(require 't)

(fff 'xxx 000)


