#lang racket/base

;; Function crosses two boundaries,
;;  only one should be blamed.

(module t typed/racket #:transient
  (define-type R Real)
  (define-type S String)

  (: g (-> (-> R R R) R (-> R R R)))
  (define (g x z) x)

  (: h (-> (-> R R S) R S))
  (define (h y zz) (y 2 1))

  (provide g h))

(require 't)

(define cnst42
  (lambda (n m) 42))

(void (g cnst42 33))
(h cnst42 44)

