#lang racket/base

;; This module provides helper macros for `require/typed`

(require racket/contract/region racket/contract/base
         "transient-contract.rkt"
         syntax/location
         (only-in (submod typed-racket/typecheck/internal-forms forms) transient-require-internal)
         (for-syntax racket/base
                     syntax/parse
                     syntax/location
                     (only-in syntax/srcloc build-source-location-list)
                     (only-in typed-racket/typecheck/internal-forms internal)
                     (only-in "tc-utils.rkt" current-type-enforcement-mode)))

(provide require/contract define-ignored rename-without-provide)

(define-syntax (define-ignored stx)
  (syntax-case stx ()
    [(_ name expr)
     (syntax-case (local-expand/capture-lifts #'expr
                                              'expression
                                              null #;(list #'define-values))
       (begin define-values)
       [(begin (define-values (n) e) ... e*)
        #`(begin (define-values (n) e) ...
                 (define name #,(syntax-property #'e*
                                                 'inferred-name
                                                 (syntax-e #'name))))]
       [(begin e)
        #`(define name #,(syntax-property #'e
                                          'inferred-name
                                          (syntax-e #'name)))])]))

;; Define a rename-transformer that's set up to avoid being provided
;; by all-defined-out or related forms.
(define-syntax (rename-without-provide stx)
  (syntax-parse stx
    [(_ nm:id hidden:id)
     #'(define-syntax nm
         (make-rename-transformer
          (syntax-property (syntax-property (quote-syntax hidden)
                                            'not-free-identifier=? #t)
                           'not-provide-all-defined #t)))]))

;; Requires an identifier from an untyped module into a typed module
;; nm is the import
;; hidden is an id that will end up being the actual definition
;; nm will be bound to a rename transformer so that it is not provided
;; with all-defined-out
(define-syntax (require/contract stx)
  (define-syntax-class renameable
    (pattern nm:id
             #:with orig-nm #'nm
             #:with orig-nm-r ((make-syntax-introducer) #'nm))
    (pattern (orig-nm:id nm:id)
             #:with orig-nm-r ((make-syntax-introducer) #'nm)))

  (syntax-parse stx
    [(require/contract nm:renameable hidden:id cnt lib orig-ty-stx)
     (define te-mode (current-type-enforcement-mode))
     #`(begin (require (only-in lib [nm.orig-nm nm.orig-nm-r]))
              (rename-without-provide nm.nm hidden)

              (define-ignored hidden
                #,(case te-mode
                    [(guarded)
                     #`(contract cnt
                                 #,(get-alternate #'nm.orig-nm-r)
                                 '(interface for #,(syntax->datum #'nm.nm))
                                 (current-contract-region)
                                 (quote nm.nm)
                                 (quote-srcloc nm.nm))]
                    [else
                     (get-alternate #'nm.orig-nm-r)]))
              #,(when (eq? 'transient te-mode)
                  ;; transient later turns this to an assert, cannot write the
                  ;; assert now because type->sexp isn't ready
                  (internal
                    #`(transient-require-internal
                        #,(get-alternate #'nm.orig-nm-r)
                        cnt
                        orig-ty-stx
                        '#,(build-source-location-list #'nm.nm)
                        (#%plain-app
                         list 'boundary 'require/typed
                         (#%variable-reference)
                         'lib
                         (current-contract-region))))))]))

;; identifier -> identifier
;; get the alternate field of the renaming, if it exists
(define-for-syntax (get-alternate id)
  (define-values (v new-id) (syntax-local-value/immediate id (λ _ (values #f #f))))
  (cond [(rename-transformer? v)
         (get-alternate (rename-transformer-target v))]
        [else id]))
