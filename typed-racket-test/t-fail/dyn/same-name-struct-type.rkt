#;
(exn-pred #rx"incompatible struct types with the same name")
#lang racket/load

;; Test the error message for subtyping errors on struct types
;; with the same name

(module a typed/racket #:transient
  (struct foo ([x : Integer]))
  (define a-foo (foo 3))
  (provide a-foo))

(module b typed/racket #:transient
  (struct foo ([x : String]))
  (define (f [a-foo : foo]) (foo-x a-foo))
  (provide f))

(module c typed/racket #:transient
  (require 'a 'b)
  (f a-foo))
