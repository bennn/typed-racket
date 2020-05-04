#lang racket

;;bg NO ERROR

(module example typed/racket #:transient
  ;; coerces into unapplicable function
  (: id (Procedure -> Procedure))
  (define (id x) x)

  (: f (Integer -> Integer))
  (define (f x) (+ x 1))

  (define g (id f))

  ;; contract here should make sure g is not applicable
  (provide g))

(require 'example)

;; g should now be unapplicable via case-> contract
(g 3)
