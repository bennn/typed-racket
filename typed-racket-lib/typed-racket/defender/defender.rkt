#lang racket/base

;; TODO
;;  - [ ] build + test occurrence-type optimizer
;;  - [ ] build/test erasure-racket
;;
;; TODO
;; - [ ] need with-new-name-tables here?
;; - [ ] syntax-track-origin ? syntax/loc/track-origin ?

(require
  (only-in racket/format ~a)
  racket/match
  syntax/id-set
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
    generate-temporary)
  (only-in (submod typed-racket/private/type-contract test-exports)
    has-contract-def-property?
    type->contract)
  (for-syntax
    racket/base)
  (for-template
    racket
    racket/unsafe/ops
    typed-racket/types/numeric-predicates))
 
(provide defend-top)

(module+ test
  (require rackunit))

;; =============================================================================

(define (defend-top stx ctc-cache sc-cache extra-defs*)
  (let loop ([stx stx] [skip-dom? #f])
    (syntax-parse stx
     #:literals (values define-values #%plain-app begin define-syntaxes letrec-values)
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
            (~literal module)
            (~literal module*)) . _)
      ;; ignore the same things the optimizer ignores
      stx]
     [(~and _:kw-lambda^ ((~literal let-values) ([(f) fun]) body))
      stx
      #;(syntax/loc stx (let-values ([(f) fun]) body))]
     [(~and _:opt-lambda^ ((~literal let-values) ([(f) fun]) body))
      stx
      #;(syntax/loc stx (let-values ([(f) fun]) body))]
     [(op:lambda-identifier formals . body)
      (define dom-map (type->domain-map (stx->arrow-type stx)))
      (define body+ (loop #'body #f))
      (void (readd-props! body+ #'body))
      (define formals+
        (if skip-dom? '() (protect-formals dom-map #'formals ctc-cache sc-cache extra-defs*)))
      (define stx+
        (with-syntax ([body+ body+])
          (if (null? formals+)
            (syntax/loc stx (op formals . body+))
            (let ((stx (quasisyntax/loc stx (op formals (#%plain-app void . #,formals+) . body+))))
              (register-ignored! (caddr (syntax-e stx)))
              stx))))
      (void (readd-props! stx+ stx))
      stx+]
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
          (#%plain-app (letrec-values (((a) e0+)) b) e1*+ ...)))]
     [(x* ...)
      #:when (is-application? stx)
      (define stx+
        (syntax*->syntax stx
          (for/list ([x (in-list (syntax-e #'(x* ...)))])
            (define x+ (loop x #f))
            (readd-props! x+ x)
            x+)))
      (void
        (readd-props! stx+ stx))
      (define-values [pre* f post*] (split-application stx+))
      (if (or (is-ignored? f)
              (blessed-codomain? f)
              (cdr-list? f post*))
        stx+
        (let ()
          (define cod-tc-res (maybe-type-of stx))
          (define stx/dom
            (with-syntax ([(pre ...) pre*]
                          [f f]
                          [post* post*])
              (syntax/loc stx+ (pre ... f . post*))))
          (add-typeof-expr stx/dom (ret Univ)) ;; TODO we can do better!
          (define stx/cod
            (protect-codomain cod-tc-res stx/dom ctc-cache sc-cache extra-defs*))
          (readd-props! stx/cod stx)
          stx/cod))]
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
      (readd-props! stx+ stx)
      stx+]
     [_
      stx])))

(define-syntax-class lambda-identifier
  (pattern (~literal #%plain-lambda))
  (pattern (~literal lambda)))

(define (readd-props! new-stx old-stx)
  (maybe-add-typeof-expr new-stx old-stx)
  (maybe-add-test-position new-stx old-stx)
  (void))

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

(define (stx->arrow-type stx [num-args #f])
  (define raw-type (tc-results->type1 (maybe-type-of stx)))
  (let loop ([ty (and raw-type (normalize-type raw-type))])
    (match ty
     [(Fun: (list (? Arrow?)))
      ty]
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
     [(Poly: _ b)
      (loop b)]
     [(Fun: arrs)
      ;;bg; if case->, try combining the arrs to a union type
      ;;    this is possible when each `arr` has the same arity
      (define arr (combine-arrs arrs))
      (if arr
        (loop (make-Fun (list arr)))
        (raise-arguments-error 'stx->arrow-type "failed to parse arrow from case->"
          "type" ty
          "e" (syntax->datum stx)
          "stx" stx
          "cases" arrs))]
     [(Union: _ ts)
      ;; TODO okay to pick arbitrary?
      ;;  example type: (U (-> (Array Integer) (values (Indexes : (Top | Bot) : (struct-ref (0 0) 0)) (Index : (Top | Bot) : (struct-ref (0 0) 1)) (Index : (Top | Bot) : (vector-length (struct-ref (0 0) 0))) (Indexes : (Top | Bot)) ((-> Indexes Integer) : (Top | Bot) : (struct-ref (0 0) 4)))) (All (A) (-> (Array A) (values (Indexes : (Top | Bot) : (Array-shape (0 0))) (Index : (Top | Bot) : (Array-size (0 0))) (Index : (Top | Bot) : (vector-length (Array-shape (0 0)))) (Indexes : (Top | Bot)) ((-> Indexes A) : (Top | Bot) : (Array-unsafe-proc (0 0)))))))
      (loop (car ts))]
     [_
      (raise-arguments-error 'stx->arrow-type "failed to parse arrow from type of syntax object"
        "e" (syntax->datum stx)
        "stx" stx
        "type" ty)])))

;; combine-arrs : (-> (Listof Arrow) (U #f Arrow))

(module+ test
  (check-equal?  ;;bg; not happy about ~a ....
    (~a (combine-arrs (list
        (make-Arrow (list -Symbol -True (make-Listof -Symbol) Univ) #f '() (-values (list -String)))
        (make-Arrow (list -False -False (make-Listof -Symbol) Univ) #f '() (-values (list -String)))
        (make-Arrow (list -Symbol Univ (make-Listof -Symbol) -True) #f '() (-values (list -String)))
        (make-Arrow (list -Symbol Univ -False -False) #f '() (-values (list -String))))))
    (~a (make-Arrow (list (Un -Symbol -False)
                          (Un -True -False Univ)
                          (Un (make-Listof -Symbol) -False)
                          (Un Univ -True -False))
                    #f '() (-values (list -String)))))
)

(define (combine-arrs arrs)
  (match arrs
   [(list (Arrow: t** #f '() rng*) ...)
    #:when (same-length? t**)
    (define m+ (combine-dom* t**))
    (define rng+ (combine-rng* rng*))
    (make-Arrow m+ #f '() rng+)]
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
         (make-Values t*))))

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
   [(or (Fun: (list (Arrow: mand rst kws _)))
        (Arrow: mand rst kws _))
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
              (hash-set t# REST-KEY
                (let ([tys (Rest-tys rst)])
                  (if (and (not (null? tys)) (null? (cdr tys)))
                    (make-Listof (car tys))
                    (raise-arguments-error 'type->domain-map "cannot handle rest type yet" "rest" tys "orig type" t))))]
             [(RestDots? rst)
              (raise-arguments-error 'type->domain-map "type without rest-dots"
                "type" t)]
             [else
              t#])]
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
    (raise-arguments-error 'type-map-ref "unbound key" "key" key "map" map))
  (cond
   [(fixnum? key)
    (hash-ref map key (λ () (hash-ref map REST-KEY fail-thunk)))]
   [(keyword? key)
    (hash-ref map key fail-thunk)]
   [(eq? REST-KEY key)
    (hash-ref map key fail-thunk)]
   [else
    (raise-argument-error 'type-map-ref "(or/c fixnum? 'rest keyword?)" 1 map key)]))

;;; type->codomain-type : Type Syntax -> (U #f SomeValues)
;;; Get the codomain from an arrow type,
;;;  use `stx` to decide whether we can skip the codomain check.
;;; 2020-03 : unused!
;(define (type->codomain-type t stx)
;  (match t
;   [(Fun: (list (Arrow: _ _ _ cod)))
;    (if (blessed-codomain? stx)
;      #f
;      cod)]
;   [_
;    (raise-argument-error 'type->cod-type "arrow type" t)]))

(define (blessed-codomain? stx)
  (if (identifier? stx)
    (or (syntax-property stx 'constructor-for) ;; 2020-03: could register in env/lexical-env instead
        ;; 2020-03 : struct predicates handled earlier in type checker
        (transient-trusted-positive? stx)
        (and (typed-racket-identifier? stx)
             (not (struct-accessor? stx))
             (not (from-require/typed? stx))))
    (is-lambda? stx)))

(define (cdr-list? f post*)
  ;; TODO put this in optimizer?
  ;; TODO bless cddr ..., but check for N-level pair type too
  (and
    (identifier? f)
    (or (free-identifier=? f #'cdr)
        (free-identifier=? f #'unsafe-cdr))
    (let* ((e (syntax-e post*))
           (t (and (pair? e) (tc-results->type1 (maybe-type-of (car e))))))
      (match t
       [(or (Listof: _)
            (Pair: _ (Listof: _))
            (Pair: _ (Pair: _ _)))
        #true]
       [_
        #false]))))

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

(define (protect-domain dom-type dom-stx ctc-cache sc-cache extra-defs*)
  (define ctc-stx
    (and dom-type (type->flat-contract dom-type ctc-cache sc-cache extra-defs*)))
  (cond
   [(not ctc-stx)
    #f]
   [else
    (define err-msg
      (parameterize ([error-print-width 20])
        (format "~e : ~a" (#%plain-app syntax->datum dom-stx) dom-type)))
    ;; TODO register ignored
    (with-syntax ([ctc ctc-stx]
                  [err err-msg]
                  [dom dom-stx])
      (define new-stx
        (syntax/loc dom-stx
          (if (#%plain-app ctc dom)
            '#true
            (#%plain-app error 'transient-assert (#%plain-app format #;'"die" '"got ~s in ~a" dom 'err)))))
      (register-ignored! new-stx)
      new-stx)]))

;; protect-codomain : (U #f Tc-Results) (Syntaxof List) Hash Hash (Boxof Syntax) -> (Syntaxof List)
(define (protect-codomain cod-tc-res app-stx ctc-cache sc-cache extra-defs*)
  (define t* (tc-results->type* cod-tc-res))
  (cond
   [(or (not cod-tc-res) (not t*))
    app-stx]
   [(null? t*)
    #;(raise-argument-error 'protect-codomain "non-empty tc-results" cod-tc-res)
    app-stx]
   [else
    (define ctc-stx* ;; (Listof (U #f Syntax))
      (for/list ([t (in-list t*)])
        (type->flat-contract t ctc-cache sc-cache extra-defs*)))
    (define err-msg
      (parameterize ([error-print-width 20])
        (format "~e : ~a" (#%plain-app syntax->datum app-stx) t*)))
    (if (not (ormap values ctc-stx*))
      ;; Nothing to check
      app-stx
      ;; Assemble everything into a syntax object that:
      ;; - performs the application
      ;; - binds the result(s) to temporary variable(s)
      ;; - checks the tag of each temporary
      (with-syntax ([app app-stx]
                    [err err-msg])
        (define var-name 'dyn-cod)
        (if (null? (cdr t*))
          ;; -- application returns 1 result, just bind it and check it
          (with-syntax ([(ctc) ctc-stx*]
                        [v (generate-temporary var-name)])
            (define new-stx
              (with-type
                cod-tc-res
                (syntax/loc app-stx
                  (let-values ([(v) app])
                    (if (#%plain-app ctc v)
                      v
                      (#%plain-app error 'transient-assert (#%plain-app format #;'"die" '"got ~s in ~a" v 'err)))))))
            (define if-stx (caddr (syntax-e new-stx)))
            (register-ignored! if-stx)
            (define chk-stx (syntax-e (cadr (syntax-e if-stx))))
            (register-ignored! chk-stx)
            (test-position-add-true chk-stx)
            (test-position-add-false chk-stx)
            (register-ignored! (caddr (syntax-e if-stx)))
            (register-ignored! (cadddr (syntax-e if-stx)))
            new-stx)
          ;; - application returns +1 results:
          ;;   - bind all,
          ;;   - check the ones with matching contracts,
          ;;   - return all
          (with-syntax ([v* (for/list ([_t (in-list t*)])
                               ;; should be OK to do this instead of `generate-temporaries`, right?
                               (generate-temporary var-name))])
            (define new-stx
              (with-type
                cod-tc-res
                (quasisyntax/loc app-stx
                  (let-values ([v* app])
                    (if #,(make-and-stx
                            app-stx
                            (for/list ([ctc-stx (in-list ctc-stx*)]
                                       [v (in-list (syntax-e #'v*))]
                                       #:when ctc-stx)
                                (register-ignored! ctc-stx)
                                (test-position-add-true ctc-stx)
                                (test-position-add-false ctc-stx)
                                (quasisyntax/loc app-stx (#%plain-app #,ctc-stx #,v))))
                      (#%plain-app values . v*)
                      (#%plain-app error 'transient-assert 'err))))))
            (register-ignored! (caddr (syntax-e new-stx)))
            new-stx))))]))

(define (make-and-stx loc stx*)
  ;; TODO awkward, remove?
  (for/fold ((acc (syntax/loc loc '#true)))
            ((stx (in-list stx*)))
    (quasisyntax/loc loc (if #,stx #,acc '#false))))

(define-syntax-rule (with-type t e)
  (let ((v e))
    (add-typeof-expr v t)
    v))

;; protect-formals : TypeMap (Syntaxof List) Hash Hash (Boxof Syntax) -> (Syntaxof List)
(define (protect-formals dom-map formals ctc-cache sc-cache extra-defs*)
  (filter values
    (let loop ([dom* formals] [position 0])
      ;;  wow this is off the hook  ... sometimes called with (a b (c . d))
      (cond
       [(null? dom*)
        '()]
       [(not (pair? dom*))
        (cond
         [(identifier? dom*)
          (define t (type-map-ref dom-map REST-KEY))
          (list (protect-domain t (datum->syntax formals dom*) ctc-cache sc-cache extra-defs*))]
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
        (cons (protect-domain t var ctc-cache sc-cache extra-defs*)
              (loop (cdr dom*) (+ position 1)))]))))

(define (formal->var stx)
  (syntax-parse stx
   [_:id
    stx]
   [(x:id _)
    (syntax/loc stx x)]))

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
    (raise-user-error 'some-values->type* "cannot generate contract for ValuesDots type '~a'" sv)]))

(define (is-lambda? x)
  (syntax-parse x
   [((~or (~literal lambda) (~literal #%plain-lambda)) . _) #true]
   [_ #false]))

(define (has-type-annotation? x)
  (match (maybe-type-of x)
   [(tc-results: _ #f)
    ;; #f = don't handle rest dots
     #true]
   [a
     #false]))

#;(define (needs-domain-check? t)
  ;; TODO recursion is similar to `function-type?` in `type-contract.rkt`
  (match t
   [(Fun: arrs)
    (ormap arr/non-empty-domain? arrs)]
   [(Union: _ elems)
    (ormap needs-domain-check? elems)]
   [(Intersection: elems _)
    (andmap needs-domain-check? elems)]
   [(Poly: _ body)
    (needs-domain-check? body)]
   [(PolyDots: _ body)
    (needs-domain-check? body)]
   [_ #f]))

(define (arr/non-empty-domain? arr)
  (match arr
   [(Arrow: '() #f '() _)
    #false]
   [(Arrow: _ _ _ _)
    #true]
   [_
    (raise-argument-error 'arr/non-empty-domain "Arrow?" arr)]))

(define (syntax*->syntax ctx stx*)
  (datum->syntax ctx
    (if (null? stx*)
      '()
      (cons (car stx*) (syntax*->syntax ctx (cdr stx*))))))

(define (type->flat-contract t ctc-cache sc-cache extra-defs*)
  (define (fail #:reason r)
    (raise-user-error 'type->flat-contract "failed to convert type ~a to flat contract because ~a" t r))
  (match-define (list defs ctc)
    (type->contract t fail
      #:typed-side #f
      #:cache ctc-cache
      #:sc-cache sc-cache))
  (for-each register-ignored! defs)
  (set-box! extra-defs* (append (reverse defs) (unbox extra-defs*)))
  (if (or (free-identifier=? ctc #'any/c)
          (free-identifier=? ctc #'none/c))
    #f
    ctc))
