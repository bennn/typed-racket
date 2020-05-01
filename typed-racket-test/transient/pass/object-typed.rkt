#lang racket

(module t typed/racket #:transient
  (define-type C% (Class (f (-> Integer Integer))))
  (: c% C%)
  (define c%
    (class object%
      (super-new)
      (define/public (f x) x)))
  (provide c%))

(require 't rackunit)

(define o (new c%))

(check-exn exn:fail:contract?
  (lambda () (send o f 'NaN)))

