#;
(exn-pred exn:fail:syntax? #rx"free variables")

#lang racket/load
(require typed/racket/base)

(: f (All (a) (Number -> (Any -> Boolean : a))))
(define (f x) (make-predicate a))
