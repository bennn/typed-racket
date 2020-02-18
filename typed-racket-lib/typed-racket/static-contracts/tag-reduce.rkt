#lang racket/base

;; 

(provide
  static-contract->tag/sc
  ;; (-> static-contract? static-contract?)
)

(require
  (for-syntax racket/base)
  "utils.rkt"
  "structures.rkt"
  racket/match)

;; =============================================================================

(define (static-contract->tag/sc sc)
  (unless (sc? sc)
    (raise-argument-error 'static-contract->tag/sc "sc?" sc))
  (log-static-contract-info "begin (tag-reduce) ~a" sc)
  (define sc+ (sc->tag/sc sc recur))
  (log-static-contract-info "end (tag-reduce) ~a" sc+)
  sc+)

(define (recur sc) (sc->tag/sc sc recur))

