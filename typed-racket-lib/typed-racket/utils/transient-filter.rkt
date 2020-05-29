#lang racket/base

;; Runtime blame filtering
;;
;; Initially, transient blames a collection of boundaries.
;; This module uses types at runtime to check whether a boundary is irrelevant
;;  to the value at hand.

(provide
  value-type-match?
  sexp->type)

(require
  racket/match
  typed-racket/rep/values-rep
  (only-in (submod typed-racket/private/type-contract test-exports) type->contract)
  typed-racket/types/numeric-tower
  typed-racket/env/type-name-env
  typed-racket/env/global-env
  typed-racket/env/type-alias-env
  typed-racket/types/struct-table
  typed-racket/types/abbrev)

(define-namespace-anchor nsa)
(define ns (namespace-anchor->namespace nsa))

;; Parse type,
;;  follow `elim-path` into the type,
;;  check whether value satisfies the transient contract at the end of the road
(define (value-type-match? val ty-datum elim-path ctx)
  (define ty-full (sexp->type ty-datum))
  (define ty-path
    (let* ((ty-full (sexp->type ty-datum)))
      (let loop ((ty ty-full)
                 (elim* elim-path))
        (if (null? elim*)
          ty
          (let ((ty+ (type-step ty (car elim*))))
            (if ty+
              (loop ty+ (cdr elim*))
              (begin
                (printf
                  "transient: PATH ERROR cannot follow ~s in ~s orig type ~s orig path ~s~n"
                  (car elim*) ty ty-full elim-path)
                #f)))))))
  (define ty-pred
    ;; ... unit-tests/contract-tests.rkt
    (let* ((ctc-fail (lambda (#:reason r) (raise-user-error 'type->flat-contract "failed to convert type ~a to flat contract because ~a" ty-path r)))
           (defs+ctc-stx (type->contract ty-path ctc-fail #:typed-side #f #:cache #f #:enforcement-mode 'transient)))
      (eval #`(let () #,@(car defs+ctc-stx) #,(cadr defs+ctc-stx)) ns)))
  (ty-pred val))

(define (sexp->type ty-datum)
  (eval ty-datum ns))

(define (type-step ty elim)
  (match elim ;; blame-source
   [`(dom . ,i)
    (match ty
     [(Fun: (list (Arrow: dom _ _ _)))
      (list-ref dom i)]
     [_ #f])]
   [`(rng . ,i)
    (match ty
     [(Fun: (list (Arrow: _ _ _ (Values: (list (Result: rng* _ _) ...)))))
      (list-ref rng* i)]
     [_ #f])]
   ['car
    #f]
   ['cdr
    #f]
   ['list-elem
    #f]
   ['list-rest
    #f]
   ['mcar
    #f]
   ['mcdr
    #f]
   [_
     #f]))

