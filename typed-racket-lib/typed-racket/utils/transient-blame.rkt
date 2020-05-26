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
  (only-in typed-racket/private/parse-type parse-type)
  (only-in (submod typed-racket/private/type-contract test-exports) type->contract)
  (for-template (only-in typed-racket/typed-racket do-standard-inits))
  (prefix-in t: typed-racket/typed-reader))

;; Parse `ty-str` to a type,
;;  follow `elim-path` into the type,
;;  check whether value satisfies the transient contract at the end of the road
(define (value-type-match? val ty-str elim-path ctx)
  ;; TODO get srcloc from cast-info struct ... use to parse type
  #;(void ;; TODO need this?
    (do-standard-inits))
  (define ty-path
    (let* ((ty-datum (call-with-input-string ty-str t:read))
           ;;(_ (dynamic-require (caar ctx)))
           (ty-stx (datum->syntax #f ty-datum ctx))
           (ty-full (parse-type ty-stx)))
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
           (ctc-data (type->contract ty-path ctc-fail #:typed-side #f #:cache #f #:enforcement-mode 'transient))
           (extra-stxs (car ctc-data))
           (ctc-stx (cadr ctc-data))
           (ctc-ns (ctc-namespace)))
      (eval #`(let () #,@extra-stxs #,ctc-stx) ctc-ns)))
  (f val))

(define (ctc-namespace)
  ;; TODO needs this file, for procedure predicate?
  (parameterize ([current-namespace (make-base-namespace)])
    (namespace-require 'racket/contract)
    (namespace-require 'racket/sequence)
    (namespace-require 'racket/async-channel)
    (namespace-require '(submod typed-racket/private/type-contract predicates))
    (namespace-require 'typed/racket/class)
    (current-namespace)))

(define (type-step2 ty elim)
  (match elim ;; blame-source
   ['dom
    (printf "DOM ~s ~n" ty)
    (match ty
     [(Fun: (list (Arrow: dom _ _ _)))
      (car dom)]
     [(Fun: arrs)
      (printf "fuckem ~s~n" arrs)
      #f]
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

