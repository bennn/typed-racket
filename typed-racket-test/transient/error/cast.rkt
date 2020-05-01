#lang typed/racket #:transient

;; Bad cast, expect transient-assert error

(cast 42 String)
