#lang racket/load

;; Test for PR 11172

(module a typed/racket #:transient (: n Positive-Integer) (define n 10) (provide: [n Integer]))
(module b typed/racket #:transient (define n 10) (provide: [n Integer]))
(module c typed/racket #:transient (provide: [n Integer]) (define n 10))
(module d typed/racket #:transient (provide: [n Integer]) (: n Positive-Integer) (define n 10))

(require 'a)
n

