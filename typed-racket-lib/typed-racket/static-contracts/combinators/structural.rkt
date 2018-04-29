#lang racket/base

;; Static contracts for structural contracts.
;; Ex: list/sc, vectorof/sc

(provide
  ;; also provides all combinator-structs
  empty-set/sc
  mutable-hash?/sc
  immutable-hash?/sc
  weak-hash?/sc
  empty-hash/sc
  immutable-vector?/sc
  mutable-vector?/sc)

(require "../../utils/utils.rkt"
         "../structures.rkt"
         "../constraints.rkt"
         "derived.rkt"
         "lengths.rkt"
         "simple.rkt"
         racket/match
         (for-syntax racket/base racket/syntax syntax/stx syntax/parse)
         racket/set
         racket/sequence
         (for-template racket/base
                       racket/contract/base
                       racket/set
                       racket/async-channel
                       racket/sequence
                       racket/promise
                       "../../utils/evt-contract.rkt"
                       "../../utils/hash-contract.rkt"
                       "../../utils/vector-contract.rkt"
                       "../../utils/promise-not-name-contract.rkt")
         racket/contract
         racket/async-channel)


(begin-for-syntax
  (define-syntax-class variance-keyword
    #:attributes (variance)
    [pattern (~and kw (~or #:covariant #:contravariant #:invariant))
             #:with variance (string->symbol (keyword->string (syntax-e (attribute kw))))])

  (define-syntax-class contract-category-keyword
    #:attributes (category category-stx)
    [pattern (~and kw (~or #:flat #:chaperone #:impersonator))
             #:attr category (string->symbol (keyword->string (syntax-e (attribute kw))))
             #:with category-stx (attribute category)])

  ;; TODO: Fix category when syntax parse is fixed
  (define-syntax-class argument-description
    #:attributes (variance name category category-stx)
    [pattern ((~or (~optional c:contract-category-keyword)
                   (~once :variance-keyword)) ...)
             #:attr name (generate-temporary)
             #:attr category (or (attribute c.category) 'impersonator)
             #:with category-stx (attribute category)])

  (define-syntax-class static-combinator-form
    #:attributes (name struct-name definition combinator2 ->restricts matcher provides map traverse)
    [pattern (name:id pos:argument-description ... )
             #:with struct-name (generate-temporary #'name)
             #:with matcher-name (format-id #'name "~a:" #'name)
             #:with definition
               #'(define name (λ (pos.name ...) (struct-name (list pos.name ...))))
             #:with ->restricts
               #'(lambda (v recur)
                   (for/list ([arg (in-list (combinator-args v))]
                              [kind (in-list (list 'pos.category-stx ...))])
                     (add-constraint (recur arg) kind)))
             #:attr combinator2
               #'(λ (constructor) (λ (pos.name ...) (constructor (list pos.name ...))))
             #:with matcher
               #'(define-match-expander matcher-name
                   (syntax-parser
                     [(_ pos.name ...)
                      #'(struct-name (list pos.name ...))]))
             #:with map
               #'(lambda (v f)
                   (struct-name
                     (for/list ([a (in-list (combinator-args v))]
                                [kind (in-list (list 'pos.variance ...))])
                       (f a kind))))
             #:with traverse
               #'(lambda (v f)
                   (for ([a (in-list (combinator-args v))]
                         [kind (in-list (list 'pos.variance ...))])
                     (f a kind)))
             #:with ctc
                 #`(-> #,@(stx-map (lambda (_) #'static-contract?) #'(pos ...)) static-contract?)
             #:with provides #'(begin (provide matcher-name)
                                      (provide/cond-contract [name ctc]))]
    [pattern (name:id . rest:argument-description)
             #:with struct-name (generate-temporary #'name)
             #:with matcher-name (format-id #'name "~a:" #'name)
             #:with definition #'(define name (λ args (struct-name args)))
             #:attr combinator2 #'(λ (constructor) (λ args (constructor args)))
             #:with ->restricts
               #'(lambda (v recur)
                   (for/list ([arg (in-list (combinator-args v))])
                     (add-constraint (recur arg) 'rest.category-stx)))
             #:with matcher
               #'(define-match-expander matcher-name
                   (syntax-parser
                    [(_ ctc (... ...))
                     #'(struct-name (list ctc (... ...)))]))
             #:with map
               #'(lambda (v f)
                   (struct-name
                     (for/list ([a (in-list (combinator-args v))])
                       (f a 'rest.variance))))
             #:with traverse
               #'(lambda (v f)
                   (for ([a (in-list (combinator-args v))])
                     (f a 'rest.variance)))
             #:with ctc
                 #'(->* () #:rest (listof static-contract?) static-contract?)
             #:with provides #'(begin (provide matcher-name)
                                      (provide/cond-contract [name ctc]))]))


(define-syntax (combinator-struct stx)
  (syntax-parse stx
    [(_ sc:static-combinator-form c:expr (~optional pre-constr-sc:expr #:defaults ((pre-constr-sc #'#f))) kind:contract-category-keyword)
     #:with constr-sc (if (syntax-e #'pre-constr-sc) #'pre-constr-sc #'sc.name)
     #'(begin
         (struct sc.struct-name combinator ()
           #:transparent
           #:methods gen:sc
           [(define sc-map sc.map)
            (define sc-traverse sc.traverse)
            (define (sc->contract v recur)
              (apply
               (sc.combinator2 (lambda (args) #`(c #,@args)))
               (map recur (combinator-args v))))
            (define (sc->tag/sc v recur)
              (apply constr-sc
                     (for/list ([arg (in-list (combinator-args v))])
                       (recur arg))))
            (define (sc->constraints v recur)
              (merge-restricts* 'kind.category-stx (sc.->restricts v recur)))]
           #:methods gen:equal+hash
           [(define (equal-proc a b recur)
              (and (eqv? (length (combinator-args a))
                         (length (combinator-args b)))
                   (for/and ([sub-a (in-list (combinator-args a))]
                             [sub-b (in-list (combinator-args b))])
                     (recur sub-a sub-b))))
            (define (hash-proc v recur)
              (for/fold ([hc (recur 'sc.name)])
                        ([sub (in-list (combinator-args v))])
                (bitwise-ior hc (recur sub))))
            (define (hash2-proc v recur)
              (for/fold ([hc (recur 'sc.name)])
                        ([sub (in-list (combinator-args v))])
                (bitwise-ior hc (recur sub))))]
           #:property prop:combinator-name (symbol->string 'sc.name))
         sc.matcher
         sc.definition
         sc.provides)]))


(define-syntax (combinator-structs stx)
  (syntax-parse stx
    [(_ (e ...) ...)
     #`(begin
         (combinator-struct e ...) ...)]))

(combinator-structs
  ((or/sc . (#:covariant)) or/c #:flat)
  ((and/sc . (#:covariant)) and/c #:flat)
  ((list/sc . (#:covariant)) list/c (λ args (list-length/sc (length args))) #:flat)
  ((listof/sc (#:covariant)) listof (λ (_) list?/sc) #:flat)
  ((cons/sc (#:covariant) (#:covariant)) cons/c (λ (_x _y) cons?/sc) #:flat)
  ((struct-property/sc (#:invariant)) struct-type-property/c (λ (_x) struct-type-property?) #:impersonator)
  ((set/sc (#:covariant #:chaperone)) set/c (λ (_x) set?/sc) #:flat)
  ((vector/sc . (#:invariant)) vector/c (λ args (vector-length/sc (length args))) #:chaperone)
  ((immutable-vector/sc . (#:covariant)) immutable-vector/c (λ args (immutable-vector-length/sc (length args))) #:flat)
  ((mutable-vector/sc . (#:invariant)) mutable-vector/c (λ args (mutable-vector-length/sc (length args))) #:chaperone)
  ((vectorof/sc (#:invariant)) vectorof (λ (_x) vector?/sc) #:chaperone)
  ((immutable-vectorof/sc (#:covariant)) immutable-vectorof/c  (λ (_x) immutable-vector?/sc) #:flat)
  ((mutable-vectorof/sc (#:invariant)) mutable-vectorof/c (λ (_x) mutable-vector?/sc) #:chaperone)
  ((promise/sc (#:covariant)) promise-not-name/c (λ (_x) promise?/sc) #:chaperone) ;;bg TODO not-name?
  ((syntax/sc (#:covariant #:flat)) syntax/c (λ (_x) syntax?/sc) #:flat)
  ((hash/sc (#:invariant #:flat) (#:invariant)) hash/c (λ (_x _y)hash?/sc) #:chaperone)
  ((mutable-hash/sc (#:invariant #:flat) (#:invariant)) mutable-hash/c (λ (_x _y) mutable-hash?/sc) #:chaperone)
  ((immutable-hash/sc (#:covariant #:flat) (#:covariant)) immutable-hash/c (λ (_x _y) immutable-hash?/sc) #:flat)
  ((weak-hash/sc (#:invariant #:flat) (#:invariant)) weak-hash/c (λ (_x _y) weak-hash?/sc) #:chaperone)
  ((box/sc (#:invariant)) box/c (λ (_x) box?/sc) #:chaperone)
  ((parameter/sc (#:contravariant) (#:covariant)) parameter/c (λ (_x _y) parameter?/sc) #:chaperone)
  ((sequence/sc . (#:covariant)) sequence/c (λ args sequence?/sc) #:impersonator)
  ((channel/sc . (#:invariant)) channel/c (λ args channel?/sc) #:chaperone)
  ((continuation-mark-key/sc (#:invariant)) continuation-mark-key/c (λ (_x) continuation-mark-key?/sc) #:chaperone)
  ((evt/sc (#:covariant)) tr:evt/c (λ (_x) evt?/sc) #:chaperone) ;;bg TODO why is tr: ???
  ((async-channel/sc (#:invariant)) async-channel/c (λ (_x) async-channel?/sc) #:chaperone))

(define empty-set/sc (and/sc set?/sc (flat/sc #'set-empty?)))

(define mutable-hash?/sc (and/sc hash?/sc
                                 (flat/sc #'(λ (h) (not (immutable? h))))
                                 (flat/sc #'(λ (h) (not (hash-weak? h))))))
(define immutable-hash?/sc (and/sc hash?/sc (flat/sc #'immutable?)))
(define weak-hash?/sc (and/sc hash?/sc (flat/sc #'hash-weak?)))
(define empty-hash/sc (and/sc hash?/sc (flat/sc #'(λ (h) (zero? (hash-count h))))))
(define immutable-vector?/sc (and/sc vector?/sc
                                     (flat/sc #'immutable?)))
(define mutable-vector?/sc (and/sc vector?/sc
                                   (flat/sc #'(λ (v) (not (immutable? v))))))
