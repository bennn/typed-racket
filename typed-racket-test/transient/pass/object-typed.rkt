#lang racket

(module t typed/racket #:transient
  (define-type C% (Class (init-field (s String)) (f (-> Integer Integer))))
  (: c% C%)
  (define c%
    (class object%
      (super-new)
      (init-field s)
      (define/public (f x) (+ 4 4))))
  (provide c%))

(require 't rackunit)

(define o (new c% (s "hello")))

(check-exn exn:fail:contract?
  (lambda () (send o f 'NaN)))

(check-not-exn
  (lambda () (new c% (s 'NotString))))
