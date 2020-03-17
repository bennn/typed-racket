#lang racket/base

(require "../utils/utils.rkt"
         (utils identifier tc-utils)
         racket/match racket/list
         (except-in (types abbrev utils prop-ops)
                    -> ->* one-of/c)
         (rep type-rep prop-rep object-rep values-rep rep-utils)
         (typecheck tc-subst)
         (logic ineq)
         (contract-req))

(provide abstract-results
         combine-props
         merge-tc-results
         tc-results->values
         erase-existentials
         static-type->dynamic-type)

;; Objects representing the rest argument are currently not supported
(define/cond-contract (abstract-results results arg-names #:rest-id [rest-id #f])
  ((tc-results/c (listof identifier?)) (#:rest-id (or/c #f identifier?))
   . ->* . SomeValues?)
  (define abstracted (abstract-obj results arg-names #t))
  (tc-results->values
   (if rest-id
       (erase-identifiers abstracted (list rest-id))
       abstracted)))

(define (tc-results->values tc)
  (match (fix-results tc)
    [(tc-any-results: f)
     (-AnyValues f)]
    [(tc-results: tcrs #f)
     (make-Values
      (map (match-lambda
             [(tc-result: t ps o) (-result t ps o)])
           tcrs))]
    [(tc-results: tcrs (RestDots: dty dbound))
     (make-ValuesDots
      (map (match-lambda
             [(tc-result: t ps o) (-result t ps o)])
           tcrs)
      dty
      dbound)]))

(define (flatten-props ps)
  (let loop ([ps ps])
    (match ps
      [(list) null]
      [(cons (AndProp: ps*) ps) (loop (append ps* ps))]
      [(cons p ps) (cons p (loop ps))])))

(define/cond-contract (combine-props new-props old-props)
  ((listof Prop?) (listof Prop?)
                  . -> .
                  (values (or/c #f (listof (or/c OrProp? LeqProp?)))
                          (or/c #f (listof (or/c TypeProp? NotTypeProp?)))))
  (define-values (atoms leqs ors)
    (let ([atoms '()]
          [leqs '()]
          [ds '()])
      (let partition! ([args (append old-props new-props)])
        (match args
          [(list) (void)]
          [(cons p rst)
           (match p
             [(TrueProp:) (partition! rst)]
             [(TypeProp: obj (Refine: t raw-p))
              (partition! (list (-is-type obj t)
                                (instantiate-obj raw-p obj)))
              (partition! rst)]
             [(? TypeProp? p)
              (set! atoms (cons p atoms))
              (partition! rst)]
             [(NotTypeProp: obj (Refine: t raw-p))
              (partition! (list (-or (-not-type obj t)
                                     (negate-prop (instantiate-obj raw-p obj)))))
              (when atoms (partition! rst))]
             [(? NotTypeProp? p)
              (set! atoms (cons p atoms))
              (partition! rst)]
             [(? LeqProp? p) (set! leqs (cons p leqs))
                             (partition! rst)]
             [(? OrProp? p) (set! ds (cons p ds))
                            (partition! rst)]
             [(AndProp: ps) (partition! ps)
                            (when atoms
                              (partition! rst))]
             [(FalseProp:) (set! atoms #f)
                           (set! leqs #f)
                           (set! ds #f)])]))
      (values (and atoms (remove-duplicates atoms))
              (and leqs (remove-duplicates leqs))
              (and ds (remove-duplicates ds)))))
  (cond
    [(not atoms) (values #f #f)]
    [else
     (let loop ([worklist ors]
                [atoms atoms]
                [leqs leqs]
                [ors null])
       (match worklist
         [(cons cur rst)
          (match cur
            [(OrProp: qs)
             (let or-loop ([qs qs] [result null])
               (match qs
                 [(cons (? LeqProp? ineq) qs)
                  (match (Leqs-imply-Leq-or-not-Leq? leqs ineq)
                    [#t (loop rst atoms leqs ors)]
                    [#f (or-loop qs result)]
                    [_ (or-loop qs (cons ineq result))])]
                 [(cons q qs)
                  (let check-loop ([ps atoms])
                    (match ps
                      [(cons p ps)
                       (cond
                         [(contradiction? q p) (or-loop qs result)]
                         [(atomic-implies? p q) (loop rst atoms leqs ors)]
                         [else (check-loop ps)])]
                      [_ (or-loop qs (cons q result))]))]
                 [_ (define new-or (apply -or result))
                    (if (OrProp? new-or)
                        (loop rst atoms leqs (cons new-or ors))
                        (loop (cons new-or rst) atoms leqs ors))]))]
            [(or (? TypeProp?)
                 (? NotTypeProp?))
             (loop rst (cons cur atoms) leqs ors)]
            [(AndProp: qs) (loop (append qs rst) atoms leqs ors)]
            [(TrueProp:) (loop rst atoms leqs ors)]
            [(FalseProp:) (values #f #f)]
            [(? LeqProp?) (loop rst atoms (cons cur leqs) ors)])]
         [_ #:when (not (satisfiable-Leqs? leqs)) (values #f #f)]
         [_ (values (append leqs ors) atoms)]))]))


(define (unconditional-prop res)
  (match res
    [(tc-any-results: p) p]
    [(tc-results: tcrs _)
     (apply
      -and
      (map (match-lambda
             [(tc-result: _ (PropSet: p+ p-) _)
              (-or p+ p-)])
           tcrs))]))

(define (merge-tc-results results [ignore-propositions? #f])
  (define/match (merge-tc-result r1 r2)
    [((tc-result: t1 (and ps1 (PropSet: p1+ p1-)) o1)
      (tc-result: t2 (PropSet: p2+ p2-) o2))
     (-tc-result (Un t1 t2)
                 (if ignore-propositions?
                     ps1
                     (-PS (-or p1+ p2+) (-or p1- p2-)))
                 (if (equal? o1 o2) o1 -empty-obj))])

  (define/match (same-dty? r1 r2)
    [(#f #f) #t]
    [((RestDots: t1 dbound) (RestDots: t2 dbound)) #t]
    [(_ _) #f])
  (define/match (merge-dty r1 r2)
    [(#f #f) #f]
    [((RestDots: t1 dbound) (RestDots: t2 dbound))
     (make-RestDots (Un t1 t2) dbound)])


  (define/match (merge-two-results res1 res2)
    [((tc-result1: (== -Bottom)) res2) res2]
    [(res1 (tc-result1: (== -Bottom))) res1]
    [((tc-any-results: f1) res2)
     (-tc-any-results (-or f1 (unconditional-prop res2)))]
    [(res1 (tc-any-results: f2))
     (-tc-any-results (-or (unconditional-prop res1) f2))]
    [((tc-results: results1 dty1)
      (tc-results: results2 dty2))
     ;; if we have the same number of values in both cases
     (cond
       [(and (= (length results1) (length results2))
             (same-dty? dty1 dty2))
        (-tc-results (map merge-tc-result results1 results2)
                    (merge-dty dty1 dty2))]
       ;; otherwise, error
       [else
        (tc-error/expr "Expected the same number of values, but got ~a"
                       (if (< (length results1) (length results2))
                           (format "~a and ~a." (length results1) (length results2))
                           (format "~a and ~a." (length results2) (length results1))))])])

  (for/fold ([res (ret -Bottom)]) ([res2 (in-list results)])
    (merge-two-results res res2)))


(define (erase-existentials rep)
  (match rep
    [(Path: _ name)
     #:when (and (identifier? name)
                 (existential-id? name))
     -empty-obj]
    [else (Rep-fmap rep erase-existentials)]))

;; static-type->dynamic-type : (-> Type? Type?)
;; Weaken a type relative to the given type enforcement mode.
;; The result describes all values that can inhabit the input type at runtime.
(define (static-type->dynamic-type ty [te-mode #f])
  (case (or te-mode (current-type-enforcement-mode))
    ((guarded)
     ty)
    ((erasure)
     Univ)
    ((transient)
     (type->shape ty))
    (else
      (raise-argument-error 'static-type->dynamic-type "type-enforcement-mode?" 1 ty te-mode))))

;; TODO test
;; TODO use fmap for this?
(define (type->shape ty)
  (let loop ((ty ty))
    (match ty
      ;; Ordinary type applications or struct type names, just resolve
      [(or (App: _ _) (Name/struct:))
       ;; TODO
       ty #;(raise-user-error 'bg:todo)]
      [(Listof: _)
       ;; Listof must come before Union and Mu (but not BaseUnion --- add test to be sure)
       (-lst Univ)]
      [(or (Univ:)
           ;; (App: (Name: name _ #f) _) ;; TODO
           (Name: _ _ #f) ;; TODO
           (Bottom:)
           (Val-able: _)
           (Base-name/contract: _ _)
           (BaseUnion: _ _) ;; TODO safe?
           (SequenceTop:)
           (Mutable-VectorTop:)
           (Opaque: _)
           (Continuation-Mark-KeyTop:)
           (Prompt-TagTop:)
           (VectorTop:)
           (BoxTop:)
           (ChannelTop:)
           (Async-ChannelTop:)
           (MPairTop:)
           (ThreadCellTop:)
           (ClassTop:)
           (UnitTop:)
           (StructTypeTop:)
           (Mutable-HashTableTop:)
           (StructTop: _)
           (Weak-HashTableTop:))
       ty]
      [(Pair: _ t-cdr)
       (let cdr-loop ((t t-cdr) (list-shape (-pair Univ -Null)))
         (match t
          [(Pair: _ t-cdr)
           (cdr-loop t-cdr (-pair Univ list-shape))]
          [(== -Null)
           list-shape]
          [_
           (-pair Univ Univ)]))]
      [(Distinction: name id t)
       (make-Distinction name id (loop t))]
      [(Refinement: par p?)
       (make-Refinement (loop par) p?)]
      [(? Union?)
       (define ts+ (map loop (Union-ts ty)))
       (make-Union (Union-mask ty) (Union-base ty) ts+ (type-list->hash ts+))]
      [(Intersection: ts raw-prop elems)
       (define ts+ (map loop ts))
       (make-Intersection ts+ raw-prop (type-list->hash ts+))]
      [(Fun: arrows)
       (make-Fun (map arrow->shape arrows))]
      [(DepFun: raw-dom _ _)
       (define mand-args (make-list (length raw-dom) Univ))
       (make-Fun (list (make-Arrow mand-args #f '() (-values Univ))))]
      [(Set: _)
       (make-Set Univ)]
      [(Sequence: _)
       (make-Sequence Univ)]
      [(Immutable-HeterogeneousVector: ts)
       (make-Immutable-HeterogeneousVector (map ->Univ ts))]
      [(Immutable-Vector: _)
       (make-Immutable-Vector Univ)]
      [(Mutable-HeterogeneousVector: ts)
       (make-Mutable-HeterogeneousVector (map ->Univ ts))]
      [(Mutable-Vector: _)
       (make-Mutable-Vector Univ)]
      [(Box: _)
       (make-Box Univ)]
      [(Async-Channel: _)
       (make-Async-Channel Univ)]
      [(Promise: _)
       (make-Promise Univ)]
      [(Continuation-Mark-Keyof: _)
       (make-Continuation-Mark-Keyof Univ)]
      [(Prompt-Tagof: _ _)
       (make-Prompt-Tagof Univ Univ)]
      [(F: v)
       ;; TODO
       Univ]
      [(ThreadCell: _)
       (make-ThreadCell Univ)]
      [(Poly-unsafe: n t)
       (make-Poly n (loop t))]
      [(PolyDots-unsafe: n b)
       (make-PolyDots n (loop b))]
      [(PolyRow: _ cs t)
       (make-PolyRow cs t)]
      [(? Mu?)
       (make-Mu (loop (Mu-body ty)))]
      [(Instance: (? Name? t))
       ;; TODO
       (make-Instance t)]
      [(Instance: (Class: _ _ fields methods _ _))
       ty #;(raise-user-error 'bg:todo)]
      [(Class: row-var inits fields publics augments _)
       ty #;(raise-user-error 'bg:todo)]
      [(Unit: imports exports init-depends results)
       ty #;(raise-user-error 'bg:todo)]
      [(Struct: _ _ _ _ _ pred? _)
       ty #;(raise-user-error 'bg:todo)]
      [(StructType: s)
       ty #;(raise-user-error 'bg:todo)]
      [(Struct-Property: s)
       ty #;(raise-user-error 'bg:todo)]
      [(or (Prefab: key _) (PrefabTop: key))
       ty #;(raise-user-error 'bg:todo)]
      [(Syntax: (? Base:Symbol?))
       ty #;(raise-user-error 'bg:todo)]
      [(Syntax: t)
       ty #;(raise-user-error 'bg:todo)]
      [(Param: in out)
       ty #;(raise-user-error 'bg:todo)]
      [(Mutable-HashTable: _ _)
       (make-Mutable-HashTable Univ Univ)]
      [(Immutable-HashTable: _ _)
       (make-Immutable-HashTable Univ Univ)]
      [(Weak-HashTable: _ _)
       (make-Weak-HashTable Univ Univ)]
      [(Channel: t)
       (make-Channel Univ)]
      [(Evt: t)
       (make-Evt Univ)]
      [(Rest: (list _))
       ty #;(raise-user-error 'bg:todo)]
      [(? Rest? rst)
       ty #;(raise-user-error 'bg:todo)]
      [(? Prop? rep)
       ty #;(raise-user-error 'bg:todo)]
      [_
       ty #;(raise-arguments-error 'type->shape "Type?" ty)])))

(define (arrow->shape arrow)
  (match arrow
   [(Arrow: dom rst kws rng)
    ;; TODO shape rng (deal with results ugh)
    (make-Arrow (map ->Univ dom)
                (and rst (-lst Univ))
                kws
                rng)]))

(define (->Univ _) Univ)

(define (type-list->hash t*)
  (for/hash ((t (in-list t*))) (values t #t)))
