#lang racket/base

(require (rename-in "utils/utils.rkt")
         (for-syntax racket/base)
         (for-template racket/base)
         (private with-types type-contract syntax-properties)
         (except-in syntax/parse id)
         racket/match racket/syntax
         syntax/flatten-begin
         (types utils abbrev generalize)
         (typecheck provide-handling tc-app-helper)
         (rep type-rep)
         (for-template (base-env top-interaction))
         (utils utils tc-utils arm)
         "standard-inits.rkt"
         "tc-setup.rkt")

(provide mb-core ti-core wt-core)

(define (mb-core stx)
  (syntax-parse stx
    [(mb (~or (~optional
               (~or (~and #:optimize    (~bind [opt? #'#t])); kept for backward compatibility
                    (~and #:no-optimize (~bind [opt? #'#f]))))
              (~optional
               (~and #:with-refinements refinement-reasoning?))
              (~optional
               (~or (~and #:guarded   (~bind [te-strat #'guarded]))
                    (~and #:transient (~bind [te-strat #'transient]))
                    (~and #:erasure   (~bind [te-strat #'erasure])))))
         ...
         forms ...)
     (let ([pmb-form (syntax/loc stx (#%plain-module-begin forms ...))])
       (parameterize ([optimize? (or (and (not (attribute opt?)) (optimize?))
                                     (and (attribute opt?) (syntax-e (attribute opt?))))]
                      [with-refinements? (or (attribute refinement-reasoning?)
                                             (with-refinements?))]
                      [current-type-enforcement-mode (if (attribute te-strat) (syntax-e #'te-strat) 'guarded)])
         (tc-module/full stx pmb-form
          (λ (new-mod pre-before-code pre-after-code)
            (define ctc-cache (make-hash))
            (define sc-cache (make-hash))
            (define (change-contract-fixups/cache forms)
              (change-contract-fixups forms ctc-cache sc-cache))
            (define (change-provide-fixups/cache forms)
              (change-provide-fixups forms ctc-cache sc-cache))
            (define (defend/cache forms)
              ;;bg; TODO cannot re-use other caches because `define`s will be out of order
              (maybe-defend forms (make-hash) (make-hash)))
            (with-syntax*
             (;; pmb = #%plain-module-begin
              [(pmb . body2) new-mod]
              ;; perform the provide transformation from [Culpepper 07]
              [transformed-body (begin0 (remove-provides #'body2) (do-time "Removed provides"))]
              ;; add the real definitions of contracts on requires
              [transformed-body
               (begin0
                 (change-contract-fixups/cache (syntax->list #'transformed-body))
                 (do-time "Fixed contract ids"))]
              ;; add the real definitions of contracts on the before- and after-code
              [(before-code ...) (change-provide-fixups/cache (flatten-all-begins pre-before-code))]
              [(after-code ...) (begin0
                                  (change-provide-fixups/cache (flatten-all-begins pre-after-code))
                                  (do-time "Generated contracts"))]
              [((before-defend-code ...) . defended-body) (defend/cache #'transformed-body)]
              ;; potentially optimize the code based on the type information
              [(optimized-body ...) (maybe-optimize #'defended-body)]
              ;; add in syntax property on useless expression to draw check-syntax arrows
              [check-syntax-help (syntax-property
                                  (syntax-property
                                   #'(void)
                                   'disappeared-binding (disappeared-bindings-todo))
                                  'disappeared-use (disappeared-use-todo))])
             ;; reconstruct the module with the extra code
             ;; use the regular %#module-begin from `racket/base' for top-level printing
             (arm #`(#%module-begin 
                     #,(if (unbox include-extra-requires?) extra-requires #'(begin))
                     before-code ... before-defend-code ... optimized-body ... after-code ... check-syntax-help)))))))]))

(define (ti-core stx )
  (current-type-enforcement-mode guarded)
  (current-type-names (init-current-type-names))
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [(_ . (module . rest))
     #'(module . rest)]
    [(_ . (~and form ((~var command (static interactive-command? #f)) . _)))
     (do-standard-inits)
     ((interactive-command-procedure (attribute command.value)) #'form)]
    [(_ . form)
     ;; TODO(endobson): Remove the call to do-standard-inits when it is no longer necessary
     ;; Cast at the top-level still needs this for some reason
     (do-standard-inits)
     (tc-toplevel/full stx #'form)]))
