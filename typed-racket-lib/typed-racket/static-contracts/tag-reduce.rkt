#lang racket/base

;; 

(provide
  static-contract->tag/sc
  ;; (-> static-contract? static-contract?)
)

(require
  (for-syntax racket/base)
  "combinators.rkt"
  "utils.rkt"
  "structures.rkt"
  racket/match)

;; =============================================================================

(define (static-contract->tag/sc sc)
  (unless (sc? sc)
    (raise-argument-error 'static-contract->tag/sc "sc?" sc))
  (log-static-contract-info "begin (tag-reduce) ~a" sc)
  (define sc+ (sc-reduce sc))
  (log-static-contract-info "end (tag-reduce) ~a" sc+)
  sc+)

(define (recur sc) (sc->tag/sc sc recur))

(define (sc-reduce sc)
  (void
    ;; ... need to flatten auxiliaries in name/scs
    ;; ... 2020-02-17 when this call is missing, possible to infinite-loop (forth/command.rkt)
    ;; TODO definitely a nicer way to do this
    (reduce-name-defs! recur))
  (sc->tag/sc sc recur))

