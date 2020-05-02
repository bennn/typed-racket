#lang racket/base

;; TODO
;;  - [X] build + test occurrence-type optimizer
;;  - [ ] build/test erasure-racket, check T S E interactions + share types
;;
;; TODO
;; - [ ] need with-new-name-tables here?
;; - [ ] syntax-track-origin ? syntax/loc/track-origin ?

(require
  (only-in racket/format ~a)
  racket/match
  syntax/parse
  typed-racket/rep/type-rep
  typed-racket/rep/values-rep
  typed-racket/types/match-expanders
  (only-in typed-racket/optimizer/unboxed-let
    escapes?)
  (only-in typed-racket/env/transient-env
    transient-trusted-positive?)
  (only-in typed-racket/typecheck/internal-forms
    typed-struct
    typed-struct/exec)
  (only-in typed-racket/types/base-abbrev
    make-CyclicListof
    make-Listof)
  (only-in typed-racket/types/abbrev
    -Bottom
    -Void
    -String
    -Symbol
    -True
    -False
    ->
    -values)
  typed-racket/types/struct-table
  typed-racket/types/type-table
  typed-racket/types/union
  typed-racket/types/utils
  (only-in typed-racket/private/syntax-properties
    type-ascription-property
    type-inst-property
    ignore^
    ignore-some^
    opt-lambda^
    kw-lambda^)
  (only-in racket/syntax
    format-id
    generate-temporary
    with-syntax*)
  (only-in syntax/srcloc
    build-source-location-list)
  (only-in (submod typed-racket/private/type-contract test-exports)
    has-contract-def-property?
    type->contract)
  (for-syntax
    racket/base)
  (for-template
    racket/base
    (only-in racket/contract/base any/c)
    racket/unsafe/ops
    typed-racket/types/numeric-predicates
    typed-racket/utils/transient-contract
    (only-in racket/private/class-internal find-method/who)))
 
(provide defend-top)

(module+ test
  (require rackunit))

;; =============================================================================

(define (defend-top stx ctc-cache)
  (define rev-extra-def* (box '()))
  (define (register-extra-defs! ex*)
    (unless (null? ex*)
      (define stx (with-syntax ((ex* ex*)) #'(begin . ex*)))
      (set-box! rev-extra-def* (cons stx (unbox rev-extra-def*)))))
  (define defended-stx
    (let loop ([stx stx] [skip-dom? #f])
      (syntax-parse stx
        #:literals (#%plain-app begin case-lambda define-syntaxes define-values
                    find-method/who let-values letrec-values values)
        ;; unsound within exn-handlers^ ?
        [;; send (for objects), MUST come before ignored exprs
         (let-values ([(_) _meth])
            (let-values ([(_) _rcvr])
              (let-values (((_) (#%plain-app find-method/who _ _ _)))
                (let-values ([(_) _args] ...) _))))
         (define stx+
           (syntax*->syntax stx
             (for/list ([x (in-list (syntax-e stx))])
               (loop x #f))))
         (void (readd-props! stx+ stx))
         (define tc-res (type-of stx))
         (define-values [extra* stx/check]
           (protect-codomain tc-res stx+ (build-source-location-list stx) ctc-cache))
         (void (register-extra-defs! extra*))
         (if stx/check
           (readd-props stx/check stx)
           stx+)]
        ;; ---------------------------------------------------------------------
        [_
         #:when (or (is-ignored? stx) ;; lookup in type-table's "ignored table"
                    (has-contract-def-property? stx))
         stx]
        [(~or _:ignore^ _:ignore-some^) ;; for struct definitions ... not sure what else
         stx]
        [((~or (~literal #%provide)
               (~literal #%require)
               (~literal begin-for-syntax)
               (~literal define-syntaxes)
               (~literal module*)
               (~literal module)
               (~literal quote)
               (~literal quote-syntax)) . _)
         stx]
        [(~and (~or :kw-lambda^ :opt-lambda^)
               (let-values ([(f) fun]) body))
         stx]
        [((~or _:lambda-identifier
               case-lambda) . _)
         #:when (not (maybe-type-of stx))
         stx]
        [(op:lambda-identifier formals . body)
         ;; TODO remove stx->arrow ? maybe this can all be simpler
         (define dom-map (type->domain-map (stx->arrow-type stx)))
         (define f+
           (let-values ([(extra* f+)
                         (if skip-dom?
                           (values '() '())
                           (protect-formals dom-map #'formals (build-source-location-list stx) ctc-cache))])
             (register-extra-defs! extra*)
             f+))
         (define stx+
           (with-syntax ([body+ (readd-props (loop #'body #f) #'body)])
             (if (null? f+)
               (syntax/loc stx (op formals . body+))
               (let ([stx+ (quasisyntax/loc stx (op formals (#%plain-app void . #,f+) . body+))])
                 (register-ignored! (caddr (syntax-e stx+)))
                 stx+))))
         (readd-props stx+ stx)]
        [((~and op case-lambda) [formals* . body*] ...)
         ;; NOTE similar to the lambda case, but cannot easily share helper functions
         ;;  because risk `identifier used out of context' errors
         (define dom-map* (map type->domain-map (stx->arrow-type* stx)))
         (define stx+
           (quasisyntax/loc stx
             (op .
                 #,(for/list ([formals (in-list (syntax-e #'(formals* ...)))]
                              [body (in-list (syntax-e #'(body* ...)))]
                              [dom-map (in-list dom-map*)])
                     (define f+
                       (let-values ([(extra* f+)
                                     (if skip-dom?
                                       (values '() '())
                                       (protect-formals dom-map formals (build-source-location-list stx) ctc-cache))])
                         (register-extra-defs! extra*)
                         f+))
                     (with-syntax ([formals formals]
                                   [body+ (readd-props (loop body #f) body)])
                       (if (null? f+)
                         (syntax/loc stx [formals . body+])
                         (let ([stx+ (quasisyntax/loc stx [formals (#%plain-app void . #,f+) . body+])])
                           (register-ignored! (cadr (syntax-e stx+)))
                           stx+)))))))
         (readd-props stx+ stx)]
        [(#%plain-app (letrec-values (((a:id) e0)) b:id) e1* ...)
         #:when (free-identifier=? #'a #'b)
         ;; (for ....) combinators expand to a recursive function that does not escape,
         ;;  no need to check the domain --- use (loop e #true) to skip
         ;; TODO can the optimizer remove these checks instead?
         (define skip? (not (escapes? #'a #'e0 #false)))
         (with-syntax ((e0+ (loop #'e0 skip?))
                      ((e1*+ ...) (for/list ((e1 (in-list (syntax-e #'(e1* ...)))))
                                    (loop e1 #f))))
           (syntax/loc stx
             (#%plain-app (letrec-values (((a) e0+)) b) e1*+ ...))) ]
        [(x* ...)
         #:when (is-application? stx)
         (define stx+
           (syntax*->syntax stx
             (for/list ([x (in-list (syntax-e #'(x* ...)))])
               (loop x #f))))
         (void (readd-props! stx+ stx))
         (define-values [pre* f post*] (split-application stx+))
         (cond
           [(or (is-ignored? f)
                (blessed-codomain? f)
                (cdr-list? f post*)
                (blessed-for-function? f))
            stx+]
           [else
            (define cod-tc-res (type-of stx))
            (define-values [extra* stx/cod]
              (protect-codomain cod-tc-res stx+ (build-source-location-list stx) ctc-cache))
            (void (register-extra-defs! extra*))
            (if stx/cod
              (readd-props stx/cod stx)
              stx+)])]
        [((~and x (~literal #%expression)) _)
         #:when (type-inst-property #'x)
         stx]
        [((~literal #%expression) e)
         #:when (type-ascription-property stx)
         (define e+ (loop #'e #f))
         (void (readd-props! e+ #'e))
         (define e++
           (with-syntax ([e+ e+])
             (syntax/loc stx (#%expression e+))))
         (void (readd-props! e++ stx))
         e++]
        [_
         #:when (type-ascription-property stx)
         (raise-user-error 'defend-top "strange type-ascription ~a" (syntax->datum stx))]
        [(x* ...)
         (define stx+
           (syntax*->syntax stx
             (for/list ((x (in-list (syntax-e #'(x* ...)))))
               (define x+ (loop x #f))
               (readd-props! x+ x)
               x+)))
         (readd-props stx+ stx)]
        [_
         stx])))
  (values (reverse (unbox rev-extra-def*)) defended-stx))

(define-syntax-class lambda-identifier
  (pattern (~literal #%plain-lambda))
  (pattern (~literal lambda)))

(define (readd-props! new-stx old-stx)
  (maybe-add-typeof-expr new-stx old-stx)
  (maybe-add-test-position new-stx old-stx)
  (void))

(define (readd-props new-stx old-stx)
  (readd-props! new-stx old-stx)
  new-stx)

(define maybe-type-of
  (let ((fail (lambda () #false)))
    (lambda (e) (type-of e fail))))

(define (maybe-add-typeof-expr new-stx old-stx)
  (let ((old-type (maybe-type-of old-stx)))
    (when old-type
      (add-typeof-expr new-stx old-type))))

(define (maybe-add-test-position new-stx old-stx)
  (maybe-add-test-true new-stx old-stx)
  (maybe-add-test-false new-stx old-stx)
  (void))

(define (maybe-add-test-true new-stx old-stx)
  (when (test-position-takes-true-branch old-stx)
    (test-position-add-true new-stx))
  (void))

(define (maybe-add-test-false new-stx old-stx)
  (when (test-position-takes-false-branch old-stx)
    (test-position-add-false new-stx))
  (void))

;; -----------------------------------------------------------------------------

;; is-application? : Syntax -> Boolean
;; Returns #true if `stx` is a function application (an app that may need dynamic checking)
(define (is-application? stx)
  (syntax-parse stx
   [((~literal #%plain-app) . _)
    (has-type-annotation? stx)]
   [_
    #false]))

;; split-application : Syntax -> (Values (Syntaxof List) Syntax (Syntaxof List))
(define (split-application stx)
  (syntax-parse stx
   #:literals (#%plain-app)
   #:datum-literals (apply)
   [((~and a #%plain-app) (~and b apply) f . arg*)
    (values #'(a b) #'f #'arg*)]
   [((~and a #%plain-app) f . arg*)
    (values #'(a) #'f #'arg*)]
   [_
    (raise-argument-error 'split-application "(Syntaxof App)" stx)]))

(define (stx->arrow-type* stx)
  (define raw-type (tc-results->type1 (type-of stx)))
  (let loop ([ty (and raw-type (normalize-type raw-type))])
    (match ty
     [(Fun: arr*)
      arr*]
     [(or (Poly: _ b)
          (PolyDots: _ b))
      (loop b)]
     [(Refine: parent pred)
      (raise-user-error 'refine "~s~n ~s~n ~s~n" ty parent pred)]
     [(DepFun: _ _ _)
      (list ty)]
     [_
      (raise-arguments-error 'stx->arrow-type* "failed to parse arrow from type of syntax object"
        "e" (syntax->datum stx)
        "stx" stx
        "type" ty)])))

(define (stx->arrow-type stx [num-args #f])
  (define raw-type (tc-results->type1 (type-of stx)))
  (let loop ([ty (and raw-type (normalize-type raw-type))])
    (match ty
     [(or (Fun: (list))
          (Fun: (list (? Arrow?))))
      ty]
     [(Fun: arrs)
      ;;bg; if case->, try combining the arrs to a union type
      ;;    this is possible when each `arr` has the same arity
      (define arr (combine-arrs arrs))
      (if arr
        (make-Fun (list arr))
        (raise-arguments-error 'stx->arrow-type "failed to parse arrow from case->"
          "type" ty
          "e" (syntax->datum stx)
          "stx" stx
          "cases" arrs))]
     [(Union: _ ts)
      ;; TODO okay to pick arbitrary?
      ;;  example type: (U (-> (Array Integer) (values (Indexes : (Top | Bot) : (struct-ref (0 0) 0)) (Index : (Top | Bot) : (struct-ref (0 0) 1)) (Index : (Top | Bot) : (vector-length (struct-ref (0 0) 0))) (Indexes : (Top | Bot)) ((-> Indexes Integer) : (Top | Bot) : (struct-ref (0 0) 4)))) (All (A) (-> (Array A) (values (Indexes : (Top | Bot) : (Array-shape (0 0))) (Index : (Top | Bot) : (Array-size (0 0))) (Index : (Top | Bot) : (vector-length (Array-shape (0 0)))) (Indexes : (Top | Bot)) ((-> Indexes A) : (Top | Bot) : (Array-unsafe-proc (0 0)))))))
      (loop (car ts))]
     [(or (Poly: _ b)
          (PolyDots: _ b))
      (loop b)]
     [(Param: in out)
      (cond
       [(not num-args)
        (raise-arguments-error 'stx->arrow-type "cannot coerce parameter to arrow type, number of arguments is unknown" "type" ty "stx" stx)]
       [(= 0 num-args)
        (-> out)]
       [(= 1 num-args)
        (-> in -Void)]
       [else
        (raise-arguments-error 'stx->arrow-type "wrong number of arguments supplied to parameter" "type" ty "stx" stx "num-args" num-args)])]
     [(Refine: parent pred)
      (raise-user-error 'refine "~s~n ~s~n ~s~n" ty parent pred)]
     [(DepFun: _ _ _)
      ty]
     [_
      (raise-arguments-error 'stx->arrow-type "failed to parse arrow from type of syntax object"
        "e" (syntax->datum stx)
        "stx" stx
        "type" ty)])))

;; combine-arrs : (-> (Listof Arrow) (U #f Arrow))
;; ... isn't there a helper for this?

(module+ test
  (require (only-in typed-racket/types/subtype subtype))

  (define (type-equal? t0 t1)
    ;; wtf equal? and sub-sub fails for my tests ... whats wrong?
    #;(and (subtype t0 t1) (subtype t1 t0))
    (equal? (~a t0) (~a t1)))

  (check type-equal?
    (combine-arrs
      (list
        (make-Arrow (list -Symbol -True (make-Listof -Symbol) Univ) #f '() (-values (list -String)))
        (make-Arrow (list -False -False (make-Listof -Symbol) Univ) #f '() (-values (list -String)))
        (make-Arrow (list -Symbol Univ (make-Listof -Symbol) -True) #f '() (-values (list -String)))
        (make-Arrow (list -Symbol Univ -False -False) #f '() (-values (list -String)))))
    (make-Arrow
      (list (Un -Symbol -False)
            (Un -True -False Univ)
            (Un (make-Listof -Symbol) -False)
            (Un Univ -True -False))
      #f '() (-values (list -String))))
  (check type-equal?
    (combine-arrs
      (list
        (make-Arrow (list -Symbol -True) (make-Rest -False) '() (-values (list -Symbol)))
        (make-Arrow (list -Symbol -True) (make-Rest -False) '() (-values (list -Symbol)))))
    (make-Arrow (list -Symbol -True) (make-Rest -False) '() (-values (list -Symbol))))
)

(define (combine-arrs arrs)
  (match arrs
   [(list (Arrow: t** rst* kw* rng*) ...)
    #:when (same-length? t**)
    (define m+ (combine-dom* t**))
    (define rst+ (combine-rst* rst*))
    (define kw+ (combine-kw* kw*))
    (define rng+ (combine-rng* rng*))
    (make-Arrow m+ rst+ kw+ rng+)]
   [_
    #f]))

(define (same-length? x**)
  (or (null? x**)
      (null? (cdr x**))
      (and (= (length (car x**)) (length (cadr x**)))
           (same-length? (cdr x**)))))

(define (combine-dom* t**)
  (if (andmap null? t**)
    '()
    (cons (apply Un (map car t**)) (combine-dom* (map cdr t**)))))

(define (combine-rng* rng*)
  (define t** (map some-values->type* rng*))
  (and (same-length? t**)
       (let ([t* (combine-dom* t**)])
         (-values t*))))

(define (combine-rst* rst*)
  (for/fold ((acc (car rst*)))
            ((r (in-list (cdr rst*))))
    (match r
     [(Rest: ty*)
      (match acc
       [(Rest: ty-acc*)
        (make-Rest (map Un ty* ty-acc*))]
       [(RestDots: _ _)
        (raise-argument-error 'combine-rst* "merge-able types" rst*)]
       [#f r])]
     [(RestDots: ty nm)
      (match acc
       [(Rest: _)
        (raise-argument-error 'combine-rst* "merge-able types" rst*)]
       [(RestDots: ty-acc nm-acc)
        (if (equal? nm nm-acc)
          (make-RestDots (Un ty ty-acc) nm-acc)
          (raise-argument-error 'combine-rst* "merge-able types" rst*))]
       [#f r])]
     [#f acc])))

(define (combine-kw* kw*)
  (for/fold ((acc (car kw*)))
            ((k (in-list (cdr kw*))))
    (and (equal? acc k) acc)))

(define (tc-results->type* r)
  (match r
   [(tc-results: (list (tc-result: ts _ _) ...) #f)
    ts]
   [_
    #f]))

(define (tc-results->type1 r)
  (match r
   [(tc-result1: t)
    t]
   [_
    #f]))

(define REST-KEY 'rest)

;; type->domain-map : Type -> TypeMap
;;   where TypeMap = (HashTable (U Fixnum 'rest Keyword) (U #f Type))
;; Build a TypeMap from the domain of an arrow type.
(define (type->domain-map t)
  (match t
   [(Fun: (list))
    (make-immutable-hash)]
   [(or (Fun: (list (Arrow: mand rst kws _)))
        (Arrow: mand rst kws _)
        (app (lambda (tt) (match tt ((DepFun: mand _ _) (list mand #f '())))) (list mand rst kws)))
    (let* ([t# (make-immutable-hash)]
           [t# ;; positional arguments
            (for/fold ([acc t#])
                      ([d (in-list mand)]
                       [i (in-naturals)])
              (hash-set acc i d))]
           [t# ;; rest args
            (cond
             [(Type? rst)
              (hash-set t# REST-KEY (make-Listof rst))]
             [(Rest? rst)
              (hash-set t# REST-KEY (make-Listof Univ))]
             [(RestDots? rst)
              (hash-set t# REST-KEY (make-Listof (RestDots-ty rst)))]
             [else
              ;; need default for `((plambda: (x ...) [xs : x ... x] xs) 3 4 5)` because poly type is gone (inst'd away)
              (hash-set t# REST-KEY (make-Listof Univ))])]
           [t# ;; kwd args
            (for/fold ([acc t#])
                      ([k (in-list kws)])
              (match k
               [(Keyword: kw ty _)
                (hash-set acc kw ty)]
               [_
                (raise-arguments-error 'type->domain-map "arrow type (with good keywords)" "type" t)]))])
      t#)]
   [_
    (raise-argument-error 'type->domain-map "arrow type" t)]))

(define (type-map-ref map key)
  (define (fail-thunk)
    #;(raise-arguments-error 'type-map-ref "unbound key" "key" key "map" map)
    -Bottom)
  (cond
   [(fixnum? key)
    (hash-ref map key (λ () (hash-ref map REST-KEY fail-thunk)))]
   [(keyword? key)
    (hash-ref map key fail-thunk)]
   [(eq? REST-KEY key)
    (hash-ref map key fail-thunk)]
   [else
    (raise-argument-error 'type-map-ref "(or/c fixnum? 'rest keyword?)" 1 map key)]))

(define (blessed-codomain? stx)
  (if (identifier? stx)
    (or (syntax-property stx 'constructor-for) ;; 2020-03: could register in env/lexical-env instead
        ;; 2020-03 : struct predicates handled earlier in type checker
        (transient-trusted-positive? stx)
        ;; 2020-04 : TR ids are not always sound, see tests
        #;(and (typed-racket-identifier? stx)
             (not (struct-accessor? stx))
             (not (from-require/typed? stx))))
    (literal-function stx)))

(define (cdr-list?  f post*)
  ;; TODO put this in optimizer? / reuse optimizer?
  (define f-depth
    (and (identifier? f)
         (cond
           [(or (free-identifier=? f #'unsafe-cdr)
                (free-identifier=? f #'cdr))
            1]
           [(free-identifier=? f #'cddr)
            2]
           [(free-identifier=? f #'cdddr)
            3]
           [(free-identifier=? f #'cddddr)
            4]
           [else #f])))
  (define t
    (and f-depth
         (let ((e (syntax-e post*)))
           (and (pair? e) (tc-results->type1 (type-of (car e)))))))
  (and (Type? t)
       (let loop ((t t)
                  (d f-depth))
         (match t
          [(Listof: _)
           #true]
          [(Pair: _ t-cdr)
           (or (zero? d)
               (loop t-cdr (- d 1)))]
          [_
           #false]))))

(define (module-path-index-join* . x*)
  (let loop ((x* x*))
    (if (null? (cdr x*))
      (module-path-index-join (car x*) #f)
      (module-path-index-join (car x*) (loop (cdr x*))))))

;; Special case: no cod-check on for loop index functions
;; This may lead to unsoundness, but I don't know how else to allow
;;  (for ((x (open-input-port "aaa"))) ....)
;; Changing the type of `make-sequence` does not seem promising because the
;;  interesting parts are the return types.
(define blessed-for-function?
  (let ((for:mpi (module-path-index-join* "for.rkt" "pre-base.rkt" "private/base.rkt" 'racket/base)))
    (lambda (stx)
      (and (equal? (syntax-source-module stx) for:mpi)
           (let ((id (syntax-e stx)))
             (or (eq? id 'pos->vals)
                 (eq? id 'for-loop)))))))

;; from-require/typed? : Identifier -> Boolean
;; Typed Racket adds this property to all require/typed identifiers,
;;  see `utils/require-contract.rkt`
(define (from-require/typed? stx)
  (syntax-property stx 'not-provide-all-defined))

(define (typed-racket-identifier? stx)
  (define ib (identifier-binding stx))
  (and (pair? ib)
       (or (identifier-binding-from-this-module? ib)
           (identifier-binding-from-typed-racket-module? ib))))

(define (identifier-binding-from-this-module? ib)
  (match ib
   [(list src-mpi _src-id nom-src-mpi nom-src-id 0 0 0)
    (and (equal? src-mpi (module-path-index-join #f #f))
         (equal? src-mpi nom-src-mpi))]
   [_
    #false]))

(define (identifier-binding-from-typed-racket-module? ib)
  (match ib
   [(list src-mpi _src-id _nom-src-mpi _nom-src-id 0 0 0)
    (typed-racket-mpi? src-mpi)]
   [_
    #false]))

(define typed-racket-mpi?
  (let ([cache (make-hash)])
    (λ (mpi)
      (hash-ref! cache mpi
        (λ () ;; Typed Racket always installs a `#%type-decl` submodule
          (let* ([mpi+ (module-path-index-join '(submod "." #%type-decl) mpi)])
            (parameterize ([current-namespace (make-base-namespace)])
              (with-handlers ([exn:fail:contract? (lambda (exn) #f)])
                (and mpi+
                     (dynamic-require mpi+ #f)
                     #t)))))))))

(define (protect-domain dom-type dom-stx ctx ctc-cache)
  (define-values [extra-def* ctc-stx]
    (if dom-type
      (type->flat-contract dom-type ctc-cache)
      (values '() #f)))
  (define dom-stx+
    (cond
     [(not ctc-stx)
      #f]
     [else
      (define err-msg
        (parameterize ([error-print-width 20])
          (format "~e : ~a" (#%plain-app syntax->datum dom-stx) dom-type)))
      (with-syntax ([ctc ctc-stx]
                    [dom dom-stx]
                    [ty-str (format "~a" dom-type)]
                    [ctx ctx])
        (define new-stx
          (syntax/loc dom-stx
            (#%plain-app transient-assert dom ctc 'ty-str 'ctx)))
        (register-ignored! new-stx)
        new-stx)]))
  (values extra-def* dom-stx+))

;; protect-codomain : ???
(define (protect-codomain cod-tc-res app-stx ctx ctc-cache)
  (define t* (tc-results->type* cod-tc-res))
  (cond
   [(or (not cod-tc-res) (not t*))
    (values '() #f)]
   [(null? t*)
    (values '() #f)]
   [else
    (define-values [extra-def* ctc-stx*]
      (type->flat-contract* t* ctc-cache))
    (define cod-stx+
      (if (not (ormap values ctc-stx*))
        ;; Nothing to check
        #f
        ;; Assemble everything into a syntax object that:
        ;; - performs the application
        ;; - binds the result(s) to temporary variable(s)
        ;; - checks the tag of each temporary
        (with-syntax ([app app-stx])
          (define var-name 'dyn-cod)
          ;; - application returns +1 results:
          ;;   - bind all,
          ;;   - check the ones with matching contracts,
          ;;   - return all
          (with-syntax* ([v*
                          (for/list ([_t (in-list t*)])
                            (generate-temporary var-name))]
                         [(check-v* ...)
                          (for/list ((ctc-stx (in-list ctc-stx*))
                                     (type (in-list t*))
                                     (v-stx (in-list (syntax-e #'v*)))
                                     (i (in-naturals))
                                     #:when ctc-stx)
                            (define if-stx
                              (with-syntax ([ctc ctc-stx]
                                            [v v-stx]
                                            [ty-str (format "~a" type)]
                                            [ctx ctx])
                                #'(#%plain-app transient-assert v ctc 'ty-str 'ctx)))
                            (ignore-if-expr! if-stx)
                            if-stx)])
            (define new-stx
              (with-type
                cod-tc-res
                (quasisyntax/loc app-stx
                  (let-values ([v* app])
                    (begin
                      check-v* ...
                      (#%plain-app values . v*))))))
            (register-ignored! (caddr (syntax-e new-stx)))
            new-stx))))
    (values extra-def* cod-stx+)]))

(define (ignore-if-expr! if-stx)
  (register-ignored! if-stx)
  (register-ignored! (caddr (syntax-e if-stx)))
  (register-ignored! (cadddr (syntax-e if-stx)))
  (let ((chk-stx (syntax-e (cadr (syntax-e if-stx)))))
    (register-ignored! chk-stx)
    (test-position-add-true chk-stx)
    (test-position-add-false chk-stx))
  (void))

(define-syntax-rule (with-type t e)
  (let ((v e))
    (add-typeof-expr v t)
    v))

;; protect-formals : ???
(define (protect-formals dom-map formals ctx ctc-cache)
  (let loop ([dom* formals] [position 0])
    ;; may be called with (a b (c . d))
    (cond
     [(null? dom*)
      (values '() '())]
     [(not (pair? dom*))
      (cond
       [(identifier? dom*)
        (define t (type-map-ref dom-map REST-KEY))
        (define-values [ex* dom-stx]
          (protect-domain t (datum->syntax formals dom*) ctx ctc-cache))
        (values ex* (list dom-stx))]
       [(syntax? dom*)
        (loop (syntax-e dom*) position)]
       [else
        (raise-arguments-error 'protect-formals "strange domain element in formals"
          "elem" dom*
          "formals" formals)])]
     [(keyword? (syntax-e (car dom*)))
      (raise-arguments-error 'protect-formals "unexpected keyword in domain"
        "elem" (car dom*)
        "formals" formals)]
     [else
      (define var (formal->var (car dom*)))
      (define t (type-map-ref dom-map position))
      (define-values [ex0* dom-first] (protect-domain t var ctx ctc-cache))
      (define-values [ex1* dom-rest] (loop (cdr dom*) (+ position 1)))
      (values (append ex0* ex1*)
              (if dom-first (cons dom-first dom-rest) dom-rest))])))

(define (formal->var stx)
  (syntax-parse stx
   [_:id
    stx]
   [(x:id _)
    (syntax/loc stx x)]
   [_
    (raise-argument-error 'formal->var "(or/c #'id #'(id _))" stx)]))

;; some-values->type* : (U Type SomeValues) -> (Listof Type)
(define (some-values->type* sv)
  (match sv
   [(? Type?)
    (list sv)]
   [(Values: r*)
    (map Result-t r*)]
   [(AnyValues: _)
    (raise-user-error 'some-values->type* "cannot generate contract for AnyValues type '~a'" sv)]
   [(ValuesDots: _ _ _)
    (raise-user-error 'some-values->type* "cannot generate contract for ValuesDots type '~a'" sv)]
   [_
     (raise-argument-error 'some-values->type* "(or/c Type? SomeValues?)" sv)]))

(define (literal-function x)
  (syntax-parse x
   [((~or (~literal lambda)
          (~literal #%plain-lambda)
          (~literal case-lambda)) . _) #true]
   [_ #false]))

(define (has-type-annotation? x)
  (match (maybe-type-of x)
   [(tc-results: _ #f)
    ;; #f = don't handle rest dots TODO wait why not???? ... use  maybe-type-of only?
     #true]
   [_
     #false]))

(define (arr/non-empty-domain? arr)
  (match arr
   [(Arrow: '() #f '() _)
    #false]
   [(Arrow: _ _ _ _)
    #true]
   [_
    (raise-argument-error 'arr/non-empty-domain "Arrow?" arr)]))

(define (syntax*->syntax ctx stx*)
  ;; TODO this may be breaking structure of input stx objects
  (datum->syntax ctx
    (if (null? stx*)
      '()
      (cons (car stx*) (syntax*->syntax ctx (cdr stx*))))))

(define (type->flat-contract t ctc-cache)
  (cond
    [(eq? t Univ)
     (values '() #f)]
    [else
     (define (fail #:reason r)
       (raise-user-error 'type->flat-contract "failed to convert type ~a to flat contract because ~a" t r))
     (match-define (list defs ctc)
       (type->contract t fail #:typed-side #false #:cache ctc-cache))
     (match t
      [(Refine: _ _)
       ;; do not lift defs; they may use a local var
       ;; e.g. (lambda (a) (lambda (b : (Refine ... a b ...)) ....))
       (define ctc+ (quasisyntax/loc ctc (let-values () #,@defs #,ctc)))
       (register-ignored! ctc+)
       (values '() ctc+)]
      [_
       (define ctc+ ;; type variables make an any/c, for example
         (if (free-identifier=? ctc #'any/c) #f ctc))
       (for-each register-ignored! defs)
       (values defs ctc+)])]))

(define (type->flat-contract* t* ctc-cache)
  (for/fold ((extra-def* '())
             (ctc-stx* '())
             #:result (values (reverse extra-def*) (reverse ctc-stx*)))
            ((t (in-list t*)))
    (define-values [ex* ctc-stx] (type->flat-contract t ctc-cache))
    (values (rev-append ex* extra-def*) (cons ctc-stx ctc-stx*))))

(define (rev-append a* b*)
  (let loop ((a* a*) (b* b*))
    (if (null? a*) b* (loop (cdr a*) (cons (car a*) b*)))))

