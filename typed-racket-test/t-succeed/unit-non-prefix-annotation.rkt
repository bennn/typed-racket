#lang typed/racket/base #:transient

(require typed/racket/unit)

(define-signature sig^
  ([f : (Integer → Integer)]))

(define-unit sig@
  (import)
  (export sig^)

  (: f : Integer → Integer)
  (define (f x) (+ 1 x)))
