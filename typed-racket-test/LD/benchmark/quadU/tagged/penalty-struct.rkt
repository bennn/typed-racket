#lang typed/racket/base #:locally-defensive

;; For wrap.rkt

(provide (struct-out $penalty))

;; =============================================================================

(struct $penalty
  ([hyphens : Natural]
   [width   : Float]
) #:transparent)

