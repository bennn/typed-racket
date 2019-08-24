(module a typed/racket/base
  (#%module-begin
   (module configure-runtime '#%kernel
     (#%module-begin (#%require racket/runtime-config) (#%app configure '#f)))
   (#%require (submod typed-racket/private/type-contract predicates))
   (#%require typed-racket/utils/utils)
   (#%require (for-meta 1 typed-racket/utils/utils))
   (#%require typed-racket/utils/any-wrap)
   (#%require typed-racket/utils/struct-type-c)
   (#%require typed-racket/utils/prefab-c)
   (#%require typed-racket/utils/opaque-object)
   (#%require typed-racket/utils/evt-contract)
   (#%require typed-racket/utils/hash-contract)
   (#%require typed-racket/utils/vector-contract)
   (#%require typed-racket/utils/sealing-contract)
   (#%require typed-racket/utils/promise-not-name-contract)
   (#%require typed-racket/utils/simple-result-arrow)
   (#%require racket/sequence)
   (#%require racket/contract/parametric)
   (begin-for-syntax
    (module* #%type-decl #f (#%plain-module-begin (#%declare #:empty-namespace) (#%require typed-racket/types/numeric-tower) (#%require typed-racket/env/type-name-env) (#%require typed-racket/env/global-env) (#%require typed-racket/env/type-alias-env) (#%require typed-racket/types/struct-table) (#%require typed-racket/types/abbrev) (#%require (just-meta 0 (rename racket/private/sort raw-sort sort)) (just-meta 0 (rename racket/private/sort vector-sort! vector-sort!)) (just-meta 0 (rename racket/private/sort vector-sort vector-sort)) (only racket/private/sort)) (#%app register-type (t-quote-syntax f) (#%app make-Fun (#%app list (let-values (((temp1) (#%app list (#%app make-Box (#%app make-Box -Int)))) ((temp2) (#%app make-Values (#%app list (#%app make-Result -Int -true-propset -empty-obj))))) (if (#%app variable-reference-constant?  (#%variable-reference -Arrow30)) (#%app -Arrow28 null unsafe-undefined unsafe-undefined '#f temp1 temp2) (#%app -Arrow30 temp1 temp2)))))) (#%app register-type (t-quote-syntax f) (#%app make-Fun (#%app list (let-values (((temp3) (#%app list (#%app make-Box (#%app make-Box -Int)))) ((temp4) (#%app make-Values (#%app list (#%app make-Result -Int -true-propset -empty-obj))))) (if (#%app variable-reference-constant?  (#%variable-reference -Arrow30)) (#%app -Arrow28 null unsafe-undefined unsafe-undefined '#f temp3 temp4) (#%app -Arrow30 temp3 temp4)))))) (#%app register-type (t-quote-syntax f) (#%app make-Fun (#%app list (let-values (((temp5) (#%app list (#%app make-Box (#%app make-Box -Int)))) ((temp6) (#%app make-Values (#%app list (#%app make-Result -Int -true-propset -empty-obj))))) (if (#%app variable-reference-constant?  (#%variable-reference -Arrow30)) (#%app -Arrow28 null unsafe-undefined unsafe-undefined '#f temp5 temp6) (#%app -Arrow30 temp5 temp6)))))))))
   (begin-for-syntax (#%app add-mod!  (#%app variable-reference->module-path-index (#%variable-reference))))
   (define-values (blame1) (#%app module-name-fixup (#%app variable-reference->module-source/submod (#%variable-reference)) (#%app list)))
   (begin-for-syntax
    (#%require typed-racket/utils/redirect-contract)
    (module #%contract-defs-reference racket/base
      (#%module-begin (module configure-runtime '#%kernel (#%module-begin (#%require racket/runtime-config) (#%app configure '#f))) (#%require racket/runtime-path) (#%require (for-meta 1 racket/base)) (define-values (contract-defs-submod) (let-values (((contract-defs-submod) (let-values (((runtime?) '#t)) (#%app list 'module '(submod ".." #%contract-defs) (#%variable-reference))))) (let-values (((get-dir) void)) (#%app apply values (#%app resolve-paths (#%variable-reference) get-dir (#%app list contract-defs-submod)))))) (begin-for-syntax (#%app register-ext-files (#%variable-reference) (let-values (((contract-defs-submod) (let-values (((runtime?) '#f)) (#%app list 'module '(submod ".." #%contract-defs) (#%variable-reference))))) (#%app list contract-defs-submod)))) (#%provide contract-defs-submod)))
    (#%require (submod "." #%contract-defs-reference))
    (define-values
     (make-redirect2)
     (#%app make-make-redirect-to-contract contract-defs-submod)))
   (module*
    #%contract-defs
    #f
    (#%plain-module-begin
     (#%declare #:empty-namespace)
     (#%require (submod typed-racket/private/type-contract predicates))
     (#%require typed-racket/utils/utils)
     (#%require (for-meta 1 typed-racket/utils/utils))
     (#%require typed-racket/utils/any-wrap)
     (#%require typed-racket/utils/struct-type-c)
     (#%require typed-racket/utils/prefab-c)
     (#%require typed-racket/utils/opaque-object)
     (#%require typed-racket/utils/evt-contract)
     (#%require typed-racket/utils/hash-contract)
     (#%require typed-racket/utils/vector-contract)
     (#%require typed-racket/utils/sealing-contract)
     (#%require typed-racket/utils/promise-not-name-contract)
     (#%require typed-racket/utils/simple-result-arrow)
     (#%require racket/sequence)
     (#%require racket/contract/parametric)
     (define-values (g5) (#%app box/c/proc9 exact-integer?))
     (define-values (g6) (#%app box/c/proc9 g5))
     (define-values (generated-contract3) (let-values (((g65) g6)) (#%app build--> '->* (#%app list g65) (#%app list) '() (#%app list) '() (#%app list) '#f '#f '#f '#f '#f '#f popular-chaperone-key-id44 '#f)))
     (define-values
      (idZ3)
      (let-values (((f) (#%app coerce-contract 'define-module-boundary-contract generated-contract3))) f))
     (define-syntaxes
      (f)
      (#%app make-provide/contract-transformer (quote-syntax f) (quote-syntax idZ3) (quote-syntax f) '#f '#f (quote-syntax idX1) (quote-syntax idB4)))
     (#%provide f)
     (define-values
      (idX1 idB4)
      (#%app do-partial-app idZ3 f 'f blame1 (#%app vector '#<path:/Users/ben/code/racket/fork/extra-pkgs/typed-racket/typed-racket-test/LD/pass/TP/a.rkt> '2 '17 '42 '1) '#f))))

   ;; main
   (define-values (f) (lambda (x) (#%app unsafe-unbox (#%app unsafe-unbox x))))
   (define-syntaxes (f) (#%app make-redirect2 (quote-syntax f)))
   (define-syntaxes
    (f)
    (#%app make-typed-renaming (t-quote-syntax f) (t-quote-syntax f) 'typed))
   (#%provide f)
   (#%provide)
   (#%app void)))
