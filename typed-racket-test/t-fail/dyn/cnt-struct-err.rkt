#;
(exn-pred exn:fail:contract? #rx"transient-assert")
#lang scheme/load

(module m typed-scheme #:transient
  (define-struct: x ([f : (Number -> Number)]))
  (: my-x x)
  (define my-x (make-x (lambda: ([z : Number]) z)))
  (provide (all-defined-out)))

(module n2 scheme/base

  (require 'm scheme/match)
  (match my-x
    [(struct x (f)) (f #f)]))


(require 'n2)
