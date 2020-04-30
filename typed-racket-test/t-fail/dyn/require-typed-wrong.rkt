#;
(exn-pred exn:fail:contract? #rx"transient-assert")
#lang scheme/load

(module m typed-scheme #:transient
  (: x (Number -> Number))
  (define (x n) (add1 n))
  (provide x))

(module n typed-scheme #:transient
  (require (only-in 'm))
  (require/typed 'm [x (String -> Number)])
  (x "foo"))

(require 'n)
