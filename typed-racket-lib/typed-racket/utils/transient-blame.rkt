#lang racket/base

;; reviving types

(provide
  value-type-match?)

(require
  racket/port
  racket/match
  typed-racket/rep/type-rep
  typed-racket/rep/values-rep
  typed-racket/types/match-expanders
  (only-in (submod typed-racket/private/type-contract test-exports) type->contract)
  typed-racket/env/init-envs
  (only-in typed-racket/standard-inits do-standard-inits)

  (only-in typed-racket/rep/rep-utils predefined-type-table)

  ;; -- copied from `tc-toplevel.rkt`
  typed-racket/types/numeric-tower
  typed-racket/env/type-name-env
  typed-racket/env/global-env
  typed-racket/env/type-alias-env
  typed-racket/types/struct-table
  typed-racket/types/abbrev

  (only-in setup/collects path->module-path)
)

(define-namespace-anchor nsa)
(define ns (namespace-anchor->namespace nsa))

;; Parse `ty-str` to a type,
;;  follow `elim-path` into the type,
;;  check whether value satisfies the transient contract at the end of the road
(define (value-type-match? val ty-str elim-path ctx)
  (writeln ty-str)
  (let ((d0 (cadr (cadr (cadr (cadr ty-str))))))
    (printf "sym ~s ~s ~s~n" d0 (symbol? d0) (eq? d0 '-Symbol)))
  (parameterize ((current-namespace ns))
    ;; TODO get srcloc from cast-info struct ... use to parse type
    (define ty-path
      (let* ((ty-datum #;(simple-> (list (simple-> (list -Symbol) -Symbol)) (simple-> (list -Symbol) -Symbol))
               ty-str)
             (ty-full
               #;(simple-> (list (simple-> (list -Symbol) -Symbol)) (simple-> (list -Symbol) -Symbol))
               #;(make-Fun (list (-Arrow (list (make-Fun (list (-Arrow (list -Symbol) -Symbol))))
                                       (make-Fun (list (-Arrow (list -Symbol) -Symbol))))))
               (eval ty-datum ns)))
        (let loop ((ty ty-full)
                   (elim* elim-path))
          (if (null? elim*)
            ty
            (let ((ty+ (type-step2 ty (car elim*))))
              (if ty+
                (loop ty+ (cdr elim*))
                (begin
                  (printf
                    "transient: PATH ERROR cannot follow ~s in ~s orig type ~s orig path ~s"
                    (car elim*) ty ty-full elim-path)
                  #f)))))))
    (define f
      ;; ... unit-tests/contract-tests.rkt
      (let* ((ctc-fail (lambda (#:reason r) (raise-user-error 'type->flat-contract "failed to convert type ~a to flat contract because ~a" ty-path r)))
             (_ (printf "motherfuck ~s~n" (list
                                            (eq? -Symbol ty-path)
                                            (hash-ref predefined-type-table -Symbol #f)
                                            (hash-ref predefined-type-table ty-path #f)
                                            -Symbol
                                            (eq-hash-code -Symbol)
                                            ty-path
                                            (eq-hash-code ty-path)
                                            )))
             (ctc-data (type->contract ty-path ctc-fail #:typed-side #f #:cache #f #:enforcement-mode 'transient))
             (extra-stxs (car ctc-data))
             (ctc-stx (cadr ctc-data)))
        (eval #`(let () #,@extra-stxs #,ctc-stx) ns)))
    (f val)))

(define (type-step2 ty elim)
  (match elim ;; blame-source
   ['dom
    (printf "DOM ~s ~n" ty)
    (match ty
     [(Fun: (list (Arrow: dom _ _ _)))
      (car dom)]
     [_ #f])]
   ['cod
    (match ty
     [(Fun: (list (Arrow: _ _ _ cod)))
      cod]
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

