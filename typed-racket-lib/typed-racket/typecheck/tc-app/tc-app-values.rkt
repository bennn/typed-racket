#lang racket/unit

(require "../../utils/utils.rkt"
         "signatures.rkt"
         "utils.rkt"
         syntax/parse racket/match racket/sequence
         (typecheck signatures tc-funapp)
         (types base-abbrev abbrev type-table utils)

         (for-label racket/base))


(import tc-expr^ tc-app^)
(export tc-app-values^)

(define-literal-set values-literals #:for-label (values call-with-values))

(define-tc/app-syntax-class (tc/app-values expected)
  #:literal-sets (values-literals)
  ;; call-with-values
  (pattern ((~and op-name call-with-values) prod con)
    (match (tc/funapp #'prod #'() (tc-expr/t #'prod) null #f)
      [(tc-results: tcrs #f)
       (define r
         (tc/funapp #'con #'(prod) (tc-expr/t #'con)
                    (for/list ([tcr (in-list tcrs)])
                      (-tc-results (list tcr) #f))
                    expected))
       (define producer-ts
         (for/list ([tcr (in-list tcrs)])
           (match tcr [(tc-result: t) t])))
       (define return-t
         (match r [(tc-result1: t) t] [_ Univ])) ;;bg TODO
       (let ([gen-t (->* '() (-values producer-ts))]
             [con-t (->* producer-ts return-t)])
         (add-typeof-expr #'op-name (ret (-> gen-t con-t return-t))))
       r]
      [(tc-results: _ (? RestDots?))
       (tc-error/expr "`call-with-values` with ... is not supported")]
      [(tc-any-results: _)
       (tc/app-regular this-syntax expected)]))
  ;; special case for `values' with single argument
  ;; we just ignore the values, except that it forces arg to return one value
  (pattern ((~and op-name values) arg)
    (let ([tc-result
           (match expected
            [(or #f (tc-any-results: _))
             (single-value #'arg)]
            [(tc-result1: tp)
             (single-value #'arg expected)]
             ;; Type check the argument, to find other errors
            [_ (single-value #'arg)])])
      (define arg-type (match tc-result [(tc-result1: t) t]))
      (add-typeof-expr #'op-name (ret (-> arg-type arg-type)))
      tc-result))
  ;; handle `values' specially
  (pattern ((~and op-name values) . args)
    (match expected
      [(or (tc-results: tcrs #f)
           (bind tcrs '()))
        (define arg-types
          (for/list ([arg (in-syntax #'args)]
                     [tcr (in-list/rest tcrs #f)])
            (match (single-value arg (and tcr (-tc-results (list tcr) #f)))
              [(tc-results: (list res) #f) res])))
        (add-typeof-expr #'op-name (ret (->* arg-types (-values arg-types))))
        (-tc-results arg-types #f)])))
