ignore2 #<syntax:.../code/racket/fork/extra-pkgs/typed-racket/typed-racket-lib/typed-racket/private/type-contract.rkt:114:8 (begin (define g5 (λ (f) (and (procedure? f) (procedure-arity-includes? f (quote 1) (quote #f))))) (define-values (lifted/1) g5))>
other (#<syntax:...de/racket/fork/extra-pkgs/typed-racket/typed-racket-lib/typed-racket/typecheck/internal-forms.rkt:198:5 define-values> #<syntax:...ode/racket/fork/extra-pkgs/typed-racket/typed-racket-lib/typed-racket/base-env/prims-contract.rkt:214:32 ()> #<syntax:...de/racket/fork/extra-pkgs/typed-racket/typed-racket-lib/typed-racket/typecheck/internal-forms.rkt:199:6 (begin (quote-syntax (require/typed-internal f1 (-> Natural (Values (Boxof Symbol) Natural))) #:local) (#%plain-app values))>)
base case #<syntax:...de/racket/fork/extra-pkgs/typed-racket/typed-racket-lib/typed-racket/typecheck/internal-forms.rkt:198:5 define-values>
other ()
other (#<syntax:...de/racket/fork/extra-pkgs/typed-racket/typed-racket-lib/typed-racket/typecheck/internal-forms.rkt:199:7 begin> #<syntax:...de/racket/fork/extra-pkgs/typed-racket/typed-racket-lib/typed-racket/typecheck/internal-forms.rkt:200:8 (quote-syntax (require/typed-internal f1 (-> Natural (Values (Boxof Symbol) Natural))) #:local)> #<syntax:...de/racket/fork/extra-pkgs/typed-racket/typed-racket-lib/typed-racket/typecheck/internal-forms.rkt:201:8 (#%plain-app values)>)
base case #<syntax:...de/racket/fork/extra-pkgs/typed-racket/typed-racket-lib/typed-racket/typecheck/internal-forms.rkt:199:7 begin>
other (#<syntax:...de/racket/fork/extra-pkgs/typed-racket/typed-racket-lib/typed-racket/typecheck/internal-forms.rkt:200:9 quote-syntax> #<syntax:...ode/racket/fork/extra-pkgs/typed-racket/typed-racket-lib/typed-racket/base-env/prims-contract.rkt:214:32 (require/typed-internal f1 (-> Natural (Values (Boxof Symbol) Natural)))> #<syntax:...de/racket/fork/extra-pkgs/typed-racket/typed-racket-lib/typed-racket/typecheck/internal-forms.rkt:200:28 #:local>)
base case #<syntax:...de/racket/fork/extra-pkgs/typed-racket/typed-racket-lib/typed-racket/typecheck/internal-forms.rkt:200:9 quote-syntax>
other (#<syntax:...ode/racket/fork/extra-pkgs/typed-racket/typed-racket-lib/typed-racket/base-env/prims-contract.rkt:214:33 require/typed-internal> #<syntax f1> #<syntax:values.rkt:9:5 (-> Natural (Values (Boxof Symbol) Natural))> . #<syntax:...ode/racket/fork/extra-pkgs/typed-racket/typed-racket-lib/typed-racket/base-env/prims-contract.rkt:202:36 ()>)
base case #<syntax:...ode/racket/fork/extra-pkgs/typed-racket/typed-racket-lib/typed-racket/base-env/prims-contract.rkt:214:33 require/typed-internal>
base case #<syntax f1>
other (#<syntax:values.rkt:9:6 ->> #<syntax:values.rkt:9:9 Natural> #<syntax:values.rkt:9:17 (Values (Boxof Symbol) Natural)>)
base case #<syntax:values.rkt:9:6 ->>
base case #<syntax:values.rkt:9:9 Natural>
other (#<syntax:values.rkt:9:18 Values> #<syntax:values.rkt:9:25 (Boxof Symbol)> #<syntax:values.rkt:9:40 Natural>)
base case #<syntax:values.rkt:9:18 Values>
other (#<syntax:values.rkt:9:26 Boxof> #<syntax:values.rkt:9:32 Symbol>)
base case #<syntax:values.rkt:9:26 Boxof>
base case #<syntax:values.rkt:9:32 Symbol>
base case #<syntax:values.rkt:9:40 Natural>
base case #<syntax:...de/racket/fork/extra-pkgs/typed-racket/typed-racket-lib/typed-racket/typecheck/internal-forms.rkt:200:28 #:local>
base case #<syntax:...de/racket/fork/extra-pkgs/typed-racket/typed-racket-lib/typed-racket/typecheck/internal-forms.rkt:201:9 #%plain-app>
base case #<syntax:...de/racket/fork/extra-pkgs/typed-racket/typed-racket-lib/typed-racket/typecheck/internal-forms.rkt:201:21 values>
ignore2 #<syntax:...code/racket/fork/extra-pkgs/typed-racket/typed-racket-lib/typed-racket/utils/require-contract.rkt:55:14 (#%require (just-meta 0 (rename (quote u) f f)) (only (quote u)))>
ignore2 #<syntax:...code/racket/fork/extra-pkgs/typed-racket/typed-racket-lib/typed-racket/utils/require-contract.rkt:34:7 (define-syntaxes (f) (#%app make-rename-transformer (#%app syntax-property (#%app syntax-property (quote-syntax f1) (quote not-free-identifier=?) (quote #t)) (quote not-provide-all-defined) (quote #t))))>
ignore2 #<syntax:...code/racket/fork/extra-pkgs/typed-racket/typed-racket-lib/typed-racket/utils/require-contract.rkt:20:17 (define-values (lifted/2) (#%app module-name-fixup (#%app variable-reference->module-source/submod (#%variable-reference)) (#%app list)))>
ignore2 #<syntax:...code/racket/fork/extra-pkgs/typed-racket/typed-racket-lib/typed-racket/utils/require-contract.rkt:21:17 (define-values (f1) (#%app apply-contract lifted/1 f (quote (interface for f)) lifted/2 (quote f) (#%app kernel:srcloc (quote "<pkgs>/typed-racket-test/LD/error/values.rkt") (quote 9) (quote 3) (quote 139) (quote 1)) (quote #f)))>
other (#<syntax:values.rkt:11:1 define-values> #<syntax:values.rkt:11:0 (a b)> #<syntax:values.rkt:11:21 (#%app f1 (quote 2))>)
base case #<syntax:values.rkt:11:1 define-values>
other (#<syntax:values.rkt:11:16 a> #<syntax:values.rkt:11:18 b>)
base case #<syntax:values.rkt:11:16 a>
base case #<syntax:values.rkt:11:18 b>
base case #<syntax:/Users/ben/code/racket/fork/racket/collects/racket/private/kw.rkt:1168:26 #%app>
base case #<syntax:values.rkt:11:22 f1>
other (#<syntax quote> #<syntax:values.rkt:11:24 2>)
base case #<syntax quote>
base case #<syntax:values.rkt:11:24 2>
base case #<syntax:/Users/ben/code/racket/fork/racket/collects/racket/private/kw.rkt:1168:26 #%app>
base case #<syntax:values.rkt:13:1 unbox>
base case #<syntax:values.rkt:13:7 a>
(module values typed/racket/base
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
    (module*
     #%type-decl
     #f
     (#%plain-module-begin
      (#%declare #:empty-namespace)
      (#%require typed-racket/types/numeric-tower)
      (#%require typed-racket/env/type-name-env)
      (#%require typed-racket/env/global-env)
      (#%require typed-racket/env/type-alias-env)
      (#%require typed-racket/types/struct-table)
      (#%require typed-racket/types/abbrev)
      (#%require
       (just-meta 0 (rename racket/private/sort raw-sort sort))
       (just-meta 0 (rename racket/private/sort vector-sort! vector-sort!))
       (just-meta 0 (rename racket/private/sort vector-sort vector-sort))
       (only racket/private/sort))
      (#%app register-type (t-quote-syntax a) (#%app make-Box -Symbol))
      (#%app register-type (t-quote-syntax b) -Nat)
      (#%app
       register-type
       (t-quote-syntax f1)
       (#%app
        make-Fun
        (#%app
         list
         (let-values (((temp1) (#%app list -Nat))
                      ((temp2)
                       (#%app
                        make-Values
                        (#%app
                         list
                         (#%app -result (#%app make-Box -Symbol))
                         (#%app -result -Nat)))))
           (if (#%app
                variable-reference-constant?
                (#%variable-reference -Arrow30))
             (#%app
              -Arrow28
              null
              unsafe-undefined
              unsafe-undefined
              '#f
              temp1
              temp2)
             (#%app -Arrow30 temp1 temp2))))))
      (#%app register-type (t-quote-syntax lifted/1) -False))))
   (begin-for-syntax
    (#%app
     add-mod!
     (#%app variable-reference->module-path-index (#%variable-reference))))
   (define-values
    (blame2)
    (#%app
     module-name-fixup
     (#%app variable-reference->module-source/submod (#%variable-reference))
     (#%app list)))
   (begin-for-syntax
    (#%require typed-racket/utils/redirect-contract)
    (module #%contract-defs-reference racket/base
      (#%module-begin
       (module configure-runtime '#%kernel
         (#%module-begin
          (#%require racket/runtime-config)
          (#%app configure '#f)))
       (#%require racket/runtime-path)
       (#%require (for-meta 1 racket/base))
       (define-values
        (contract-defs-submod)
        (let-values (((contract-defs-submod)
                      (let-values (((runtime?) '#t))
                        (#%app
                         list
                         'module
                         '(submod ".." #%contract-defs)
                         (#%variable-reference)))))
          (let-values (((get-dir) void))
            (#%app
             apply
             values
             (#%app
              resolve-paths
              (#%variable-reference)
              get-dir
              (#%app list contract-defs-submod))))))
       (begin-for-syntax
        (#%app
         register-ext-files
         (#%variable-reference)
         (let-values (((contract-defs-submod)
                       (let-values (((runtime?) '#f))
                         (#%app
                          list
                          'module
                          '(submod ".." #%contract-defs)
                          (#%variable-reference)))))
           (#%app list contract-defs-submod))))
       (#%provide contract-defs-submod)))
    (#%require (submod "." #%contract-defs-reference))
    (define-values
     (make-redirect3)
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
     (#%require racket/contract/parametric)))
   (module u racket/base
     (#%module-begin
      (module configure-runtime '#%kernel
        (#%module-begin
         (#%require racket/runtime-config)
         (#%app configure '#f)))
      (#%provide f)
      (define-values (f) (lambda (x) (#%app values x x)))))
   (define-values
    (g5)
    (lambda (f)
      (if (#%app procedure? f)
        (#%app procedure-arity-includes? f '1 '#f)
        '#f)))
   (define-values (lifted/1) g5)
   (define-values
    ()
    (begin
      (quote-syntax
       (require/typed-internal f1 (-> Natural (Values (Boxof Symbol) Natural)))
       #:local)
      (#%plain-app values)))
   (#%require (just-meta 0 (rename 'u f f)) (only 'u))
   (define-syntaxes
    (f)
    (#%app
     make-rename-transformer
     (#%app
      syntax-property
      (#%app syntax-property (quote-syntax f1) 'not-free-identifier=? '#t)
      'not-provide-all-defined
      '#t)))
   (define-values
    (lifted/2)
    (#%app
     module-name-fixup
     (#%app variable-reference->module-source/submod (#%variable-reference))
     (#%app list)))
   (define-values
    (f1)
    (#%app
     apply-contract
     lifted/1
     f
     '(interface for f)
     lifted/2
     'f
     (#%app
      kernel:srcloc
      '"<pkgs>/typed-racket-test/LD/error/values.rkt"
      '9
      '3
      '139
      '1)
     '#f))
   (define-values (a b) (#%app f1 '2))
   (#%app call-with-values (lambda () (#%app unbox a)) print-values)
   (#%provide)
   (#%app void)))
