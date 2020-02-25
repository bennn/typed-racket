#lang typed/racket/base #:transient

;; Not a test, just provides a locally-defensive function

(provide f filter)

(: f (-> Real (-> Real Real) Real))
(define (f r g)
  (g (g r)))
