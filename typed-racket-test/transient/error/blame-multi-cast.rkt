#lang racket/base

;; Function crosses several boundaries,
;;  expect to see them in order at end

;; From gfd-oopsla-2019 page 24

(module u0 racket/base
  (define (f0 x0) (cons x0 x0))
  (provide f0))

(module t1 typed/racket/base #:transient
  (require/typed (submod ".." u0)
    (f0 (-> Real String)))
  (void f0))
(require 't1)

(module t2 typed/racket/base #:transient
  (require/typed (submod ".." u0)
    (f0 (-> Real Boolean)))
  (void f0))
(require 't2)

(module t3 typed/racket/base #:transient
  (require/typed (submod ".." u0)
    (f0 (-> Real Real)))
  (void f0))
(require 't3)

(module tN typed/racket/base #:transient
  (require/typed (submod ".." u0)
    (f0 (-> Real Real)))
  (f0 5))

(require 'tN)

