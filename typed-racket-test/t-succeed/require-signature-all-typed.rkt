#lang typed/racket #:transient

(module a typed/racket #:transient
  (provide foo^)

  (define-signature foo^
    ([n : Number])))

(require 'a)

(define-unit foo@
  (import)
  (export foo^)
  (define n 5))
