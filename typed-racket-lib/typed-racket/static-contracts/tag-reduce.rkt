#lang racket/base

;; 

(provide
  static-contract->tag/sc
  ;; (-> static-contract? #:contract-depth (or/c #f natural?) static-contract?)
)

(require
  (for-syntax racket/base)
  "combinators.rkt"
  "utils.rkt"
  "structures.rkt"
  racket/match)

;; =============================================================================

(define (static-contract->tag/sc sc #:contract-depth [contract-depth #f])
  (unless (sc? sc)
    (raise-argument-error 'static-contract->tag/sc "sc?" sc))
  (cond
   [(exact-nonnegative-integer? contract-depth)
    (log-static-contract-info "begin (tag-reduce ~a) ~a" contract-depth sc)
    (define sc+ ((sc-reduce contract-depth) sc 'covariant))
    (log-static-contract-info "end (tag-reduce ~a) ~a" contract-depth sc+)
    sc+]
   [else
    sc]))

(define (recur sc) (sc->tag/sc sc recur))

;; TODO keep table of all the relevant lambdas? Hmph, probably faster as-is
(define ((sc-reduce fuel) sc _variance)
  (cond
   [(zero? fuel)
    (void
      ;; ... need to flatten auxiliaries in name/scs
      ;; ... 2020-02-17 when this call is missing, possible to infinite-loop (forth/command.rkt)
      ;; TODO definitely a nicer way to do this
      (reduce-name-defs! recur))
    (sc->tag/sc sc recur)]
   [else
    (define fuel--
      (match sc
       [(or (case->/sc: _ ...)
            (arr/sc: _ _ _)
            (parametric->/sc: _ _)
            (sealing->/sc: _ _ _)
            (? recursive-sc?))
        fuel]
       [_
        (- fuel 1)]))
    (sc-map sc (sc-reduce fuel--))]))

