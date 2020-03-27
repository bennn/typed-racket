#lang typed/racket #:transient

;; Test for github issue #336

(: foo (∀ (A ... B ...) (→ (List (→ A ... B) ...)
                           Any)))
(define (foo f)
  (apply conjoin f))
