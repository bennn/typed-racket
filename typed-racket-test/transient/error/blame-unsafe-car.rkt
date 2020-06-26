#lang racket/base

;; Error with unsafe-car,
;; blame 1 boundary

(module t typed/racket/base #:transient
  (: fff (-> (Listof Bytes) Void))
  (define (fff xx)
    (void (unsafe-car xx)))
  (provide fff))

(require 't)
(fff (list 'not-bytes))

