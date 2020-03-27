#lang typed/racket/base #:transient

;; Test that VectorTop generates a flat contract

(define v : VectorTop (vector))
(void (cast v VectorTop))
