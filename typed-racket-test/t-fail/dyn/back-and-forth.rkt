#;
(exn-pred exn:fail:contract? #rx"transient-assert")

#lang scheme/load

(module m typed/scheme #:transient
  (: f (Number -> Number))
  (define (f x) (add1 x))
  (define g 17)
  (provide f g))

(module violator scheme
  (require 'm)
  (f 'foo))

(module o typed/scheme
  (require 'violator))

(require 'o)
