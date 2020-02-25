#lang typed/racket/base #:transient

(provide (struct-out $penalty))

;; -----------------------------------------------------------------------------

(require
  require-typed-check
  "../base/macro-s.rkt"
  "../base/core-types.rkt")

(require/typed/check "penalty-struct.rkt"
  [#:struct $penalty ([hyphens : Nonnegative-Integer]
                      [width : Value-Type])])

;; =============================================================================

(define-type Value-Type Float)
