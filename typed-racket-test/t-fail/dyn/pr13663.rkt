#;
(exn-pred exn:fail:contract? rx"transient-assert")
#lang racket/load

(module typed typed/racket #:transient
  (provide g)

  (: f (Byte -> Natural))
  (define (f x) (add1 x))
  (: g ((Boxof Any) -> Void))
  (define (g b) 
    (set-box! b f)))

(require 'typed)
(define b (box #f))
(g b)
(displayln ((unbox b) "foo"))
