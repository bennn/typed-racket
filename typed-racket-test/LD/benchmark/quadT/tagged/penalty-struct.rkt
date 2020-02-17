#lang typed/racket/base #:locally-defensive

(provide (struct-out $penalty))

;; -----------------------------------------------------------------------------

(require
  "../base/macro-s.rkt"
  "../base/core-types.rkt")

;; =============================================================================

(struct $penalty
  ([hyphens : Nonnegative-Integer][width : Value-Type]) #:transparent)

(define-type Value-Type Float)
