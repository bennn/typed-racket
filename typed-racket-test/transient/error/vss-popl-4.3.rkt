#lang racket/base

;; Example from vss-popl-2017 section 4.3
;; Function crosses two boundaries,
;;  only one should be blamed.

;; NOTE the paper has static type errors in `g` and `h`

(module t typed/racket #:transient
  (: g (-> (-> Real Real) (-> Real Real)))
  (define (g x) x)

  (: h (-> (-> Real String) String))
  (define (h y) (y 21))

  (provide g h))

(require 't)

(define cnst42
  (lambda (n) 42))

(void (g cnst42))
(h cnst42)

