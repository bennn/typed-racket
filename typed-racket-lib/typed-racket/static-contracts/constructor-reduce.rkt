#lang racket/base

;; Reduce a static contract from "a contract for a type"
;;  into "a contract for a type constructor"

(provide
  static-contract->constructor/c
  ;; (-> static-contract? #:contract-depth (or/c #f natural?) static-contract?)
)

(require
  (for-syntax racket/base)
  "combinators.rkt"
  "utils.rkt"
  "structures.rkt"
  racket/match)

;; =============================================================================

(define (static-contract->constructor/c sc #:contract-depth [contract-depth #f])
  (cond
   [(exact-nonnegative-integer? contract-depth)
    (log-static-contract-info "begin (tag-reduce ~a) ~a" contract-depth sc)
    (define sc+ ((sc-reduce contract-depth) sc 'covariant))
    (log-static-contract-info "end (tag-reduce ~a) ~a" contract-depth sc+)
    sc+]
   [else
    sc]))

(define (recur sc) (sc->constructor/c sc recur))

(define ((sc-reduce fuel) sc _variance)
  (cond
   [(zero? fuel)
    (sc->constructor/c sc recur)]
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
    (when (= 1 fuel)
      ;; if it's a name, need to flatten auxiliaries
      (match sc
       [(name/sc: _)
        (sc->constructor/c sc recur)]
       [_ (void)]))
    (sc-map sc (sc-reduce fuel--))]))

