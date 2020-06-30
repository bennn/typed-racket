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
         (only-in typed-racket/env/init-envs type->transient-sexp)
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
               (~or (~and #:guarded   (~bind [te-mode #'guarded]))
                    (~and #:transient (~bind [te-mode #'transient]))
                    (~and #:erasure   (~bind [te-mode #'erasure])))))
         ...
         forms ...)
     (let ([pmb-form (syntax/loc stx (#%plain-module-begin forms ...))]
           [te-mode (if (attribute te-mode) (syntax-e #'te-mode) 'guarded)])
       (parameterize ([optimize? (and (memq te-mode (list guarded transient))
                                      (if (attribute opt?) (syntax-e (attribute opt?)) (optimize?)))]
                      [with-refinements? (or (attribute refinement-reasoning?)
                                             (with-refinements?))])
         (tc-module/full te-mode stx pmb-form
          (λ (new-mod pre-before-code pre-after-code)
            (define ctc-cache (make-hash))
            (define sc-cache (make-hash))
            (define (change-contract-fixups/cache forms)
              (change-contract-fixups forms ctc-cache sc-cache))
            (define (change-provide-fixups/cache forms)
              (change-provide-fixups forms ctc-cache sc-cache))
            (define (defend/cache body-stx)
              (define-values [extra-def* body+] (maybe-defend body-stx ctc-cache sc-cache))
              (when extra-def*
                (set-box! include-extra-requires? #t))
              (cons (or extra-def* '()) body+))
            (define (add-sexp->type form*-stx)
              ;; look fo #%type-decl submod, replace a #f with a hash
              (for/list ((form (in-list (syntax-e form*-stx))))
                (syntax-parse form #:literals (begin-for-syntax module* #%plain-module-begin define let)
                 [(begin-for-syntax
                    (module* #%type-decl #f
                      (#%plain-module-begin
                       pre-form* ...
                       (define sexp->type
                         (let ((sexp->type# #f))
                           body))
                       post-form* ...)))
                  (quasisyntax/loc form
                    (begin-for-syntax
                      (module* #%type-decl #f
                        (#%plain-module-begin
                         pre-form* ...
                         (define sexp->type
                           (let ((sexp->type#
                                  (make-immutable-hash
                                    ;; TODO use the contracts here too, to avoid an eval later?
                                    #,(for/list ((ty (in-hash-keys sc-cache)))
                                        (with-syntax ((ty-datum (type->transient-sexp ty)))
                                          (syntax-local-introduce
                                            (syntax/loc form
                                              (#%plain-app cons 'ty-datum 'ty-datum))))))))
                             body))
                         post-form* ...))))]
                 [_
                   form])))
            (with-syntax*
             (;; pmb = #%plain-module-begin
              [(pmb . body2) new-mod]
              ;; perform the provide transformation from [Culpepper 07]
              [transformed-body (begin0 (remove-provides #'body2) (do-time "Removed provides"))]
              [((before-defend-code ...) . defended-body) (defend/cache #'transformed-body)]
              ;; add the real definitions of contracts on requires
              [transformed-body
               (begin0 (change-contract-fixups/cache (syntax->list #'defended-body))
                       (do-time "Fixed contract ids"))]
              ;; add the real definitions of contracts on the before- and after-code
              [(before-code ...) (change-provide-fixups/cache (flatten-all-begins pre-before-code))]
              [(after-code ...) (begin0 (change-provide-fixups/cache (flatten-all-begins pre-after-code))
                                  (do-time "Generated contracts"))]
              ;; potentially optimize the code based on the type information
              [(optimized-body ...) (maybe-optimize #'transformed-body)] ;; has own call to do-time
              ;; add in syntax property on useless expression to draw check-syntax arrows
              [check-syntax-help (syntax-property
                                  (syntax-property
                                   #'(void)
                                   'disappeared-binding (disappeared-bindings-todo))
                                  'disappeared-use (disappeared-use-todo))])
             ;; reconstruct the module with the extra code
             ;; use the regular %#module-begin from `racket/base' for top-level printing
             (arm #`(#%module-begin
                     #,(if (unbox include-extra-requires?) (extra-requires) #'(begin))
                     before-defend-code ... #,@(add-sexp->type #'(before-code ...)) optimized-body ... after-code ... check-syntax-help)))))))]))

(define (ti-core stx [te-mode guarded])
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
     (tc-toplevel/full te-mode stx #'form)]))
