#lang racket/base

;; Runtime blame filtering
;;
;; Initially, transient blames a collection of boundaries.
;; This module uses types at runtime to check whether a boundary is irrelevant
;;  to the value at hand.

(provide
  value-type-match?
  sexp->type)

(require
  racket/match
  typed-racket/rep/values-rep
  (only-in (submod typed-racket/private/type-contract test-exports) type->contract)
  typed-racket/types/numeric-tower
  typed-racket/env/type-name-env
  typed-racket/env/global-env
  typed-racket/env/type-alias-env
  typed-racket/types/struct-table
  typed-racket/types/abbrev
  ;; contract requires, need to `eval` code from type->contract
  racket/list)

(define-namespace-anchor nsa)
(define ns (namespace-anchor->namespace nsa))

;; Parse type,
;;  follow `elim-path` into the type,
;;  check whether value satisfies the transient contract at the end of the road
(define (value-type-match? val ty-datum elim-path ctx)
  (define ty-full (sexp->type ty-datum))
  (define ty-path
    (let* ((ty-full (sexp->type ty-datum)))
      (let loop ((ty ty-full)
                 (elim* elim-path))
        (if (null? elim*)
          ty
          (let ((ty+ (type-step ty (car elim*))))
            (if ty+
              (loop ty+ (cdr elim*))
              (begin
                (printf
                  "transient: PATH ERROR cannot follow ~s in ~s orig type ~s orig path ~s~n"
                  (car elim*) ty ty-full elim-path)
                #f)))))))
  (define ty-pred
    ;; ... unit-tests/contract-tests.rkt
    (let* ((ctc-fail (lambda (#:reason r) (raise-user-error 'type->flat-contract "failed to convert type ~a to flat contract because ~a" ty-path r)))
           (defs+ctc-stx (type->contract ty-path ctc-fail #:typed-side #f #:cache #f #:enforcement-mode 'transient)))
      (eval #`(let () #,@(car defs+ctc-stx) #,(cadr defs+ctc-stx)) ns)))
  (ty-pred val))

(define (sexp->type ty-datum)
  (eval ty-datum ns))

;; elim : blame-source? (see transient-contract.rkt)
(define (type-step ty elim)
  ;; NOTE the "Top" types should never happen,
  ;;  or anything else that returns Any / Univ
  ;;  but they are here to be safe --- we need the blame-trails experiment
  ;;  to run successfully ASAP and can debug extra Transient checks later
  (match elim
   [`(dom . ,i)
    (match ty
     [(Fun: (list (Arrow: dom _ _ _)))
      (list-ref/#f dom i)]
     [_ #f])]
   [`(rng . ,i)
    (match ty
     [(Fun: (list (Arrow: _ _ _ (Values: (list (Result: rng* _ _) ...)))))
      (list-ref/#f rng* i)]
     [_ #f])]
   ['car
    (match ty
     [(Listof: t)
      t]
     [(Pair: t _)
      t]
     [_
       #f])]
   ['cdr
    (match ty
     [(Listof: _)
      ty]
     [(Pair: _ t)
      t]
     [_
       #f])]
   ['list-elem
    (match ty
     [(Listof: t)
      t]
     [(List: t*)
      (apply Un t*)]
     [_
       #f])]
   ['list-rest
    (match ty
     [(Listof: _)
      ty]
     [(Pair: _ (== -Null))
      -Null]
     [(List: t*)
      (apply -lst* (cdr t*))]
     [_
       #f])]
   ['mcar
    (match ty
     [(MPair: t _)
      t]
     [(MPairTop:)
      Univ]
     [_
       #f])]
   ['mcdr
    (match ty
     [(MPair: _ t)
      t]
     [(MPairTop:)
      Univ]
     [_
       #f])]
   ['vector-elem
    (match ty
     [(Vector: t)
      t]
     [(or (Mutable-HeterogeneousVector: ts)
          (Immutable-HeterogeneousVector: ts))
      (apply Un ts)]
     [(Mutable-VectorTop:)
      Univ]
     [_
       #f])]
   ['box-elem
    (match ty
     [(Box: t)
      t]
     [(BoxTop:)
      Univ]
     [_
       #f])]
   ['hash-key
    (match ty
     [(or (Mutable-HashTable: t _)
          (Immutable-HashTable: t _)
          (Weak-HashTable: t _))
      t]
     [(or (Mutable-HashTableTop:)
          (Weak-HashTableTop:))
      Univ]
     [_
       #f])]
   ['hash-value
    (match ty
     [(or (Mutable-HashTable: _ t)
          (Immutable-HashTable: _ t)
          (Weak-HashTable: _ t))
      t]
     [(or (Mutable-HashTableTop:)
          (Weak-HashTableTop:))
      Univ]
     [_
       #f])]
   ['sequence-ref
    (match ty
     [(Sequence: t*)
      (and (pair? t*)
           (null? (cdr t*))
           (car t*))]
     [(SequenceTop:)
      Univ]
     [(SequenceDots: tys dty dbound)
      #f]
     [_
       #f])]
   ['sequence-rest
    (match ty
     [(or (Sequence: _)
          (SequenceTop:)
          (SequenceDots: _ _ _))
      ty]
     [_
       #f])]
   ['set-elem
    (match ty
     [(Set: t)
      t]
     [_
       #f])]
   ['set-rest
    (match ty
     [(Set: _)
      ty]
     [_
       #f])]
   [`(struct-elem . ,i)
    (match ty
     [(Struct: _nm _par (list (fld: t* _ _) ...) _ _ _ _)
      (list-ref/#f t* i)]
     [_
       #f])]
   [`(object-field . ,name)
     #f]
   [`(object-method-rng . ,name)
     #f]

   [_
    #f]))

(define (list-ref/#f lst i)
  (or
    (for/first ([x (in-list lst)]
                [k (in-naturals)]
                #:when (= i k))
      x)
    #f))

(module+ test
  (require rackunit)

  (test-case "type-step"
    ;; TODO lots to test with domains ... optional, keyword, etc ... what does defender do?

    (check-equal?
      (type-step (-> -Real -Symbol) '(rng . 0))
      -Symbol)
    (check-equal?
      (type-step (-> -Real (-values (list -Symbol -String))) '(rng . 0))
      -Symbol)
    (check-equal?
      (type-step (-> -Real (-values (list -Symbol -String))) '(rng . 1))
      -String)
    (check-equal?
      (type-step (-> -Real (-values (list -Symbol -String))) '(rng . 4))
      #f)
    (check-equal?
      (type-step (-> -Real (-values (list -Symbol -String))) 'rng)
      #f)

    (check-equal?
      (type-step (-lst -Symbol) 'car)
      -Symbol)
    (check-equal?
      (type-step (-lst -Symbol) 'first)
      #false)
    (check-equal?
      (type-step (-lst -Symbol) 'last)
      #false)
    (check-equal?
      (type-step (-lst -Symbol) 'list-elem)
      -Symbol)
    (check-equal?
      (type-step (-lst -Symbol) 'cdr)
      (-lst -Symbol))
    (check-equal?
      (type-step (-lst -Symbol) 'list-rest)
      (-lst -Symbol))

    (check-equal?
      (type-step (-lst* -Symbol -String) 'list-elem)
      (Un -Symbol -String))
    (check-equal?
      (type-step (-lst* -Symbol -String) 'list-rest)
      (-lst* -String))
    (check-equal?
      (type-step (-lst* -Symbol) 'list-rest)
      -Null)

    (check-equal?
      (type-step (-pair -Symbol -String) 'car)
      -Symbol)
    (check-equal?
      (type-step (-pair -Symbol -String) 'cdr)
      -String)
    (check-equal?
      (type-step (-pair -Symbol -String) 'list-elem)
      #f)
    (check-equal?
      (type-step (-pair -Symbol -String) 'list-rest)
      #f)

    (check-equal?
      (type-step (-mpair -Symbol -String) 'mcar)
      -Symbol)
    (check-equal?
      (type-step (-mpair -Symbol -String) 'mcdr)
      -String)
    (check-equal?
      (type-step (-mpair -Symbol -String) 'cdr)
      #f)
    (check-equal?
      (type-step -MPairTop 'mcar)
      Univ)
    (check-equal?
      (type-step -MPairTop 'mcdr)
      Univ)

    (check-equal?
      (type-step (-ivec -Symbol) 'vector-elem)
      -Symbol)
    (check-equal?
      (type-step (-mvec -Symbol) 'vector-elem)
      -Symbol)
    (check-equal?
      (type-step (-ivec* -Symbol -String) 'vector-elem)
      (Un -Symbol -String))
    (check-equal?
      (type-step (-mvec* -Symbol -String) 'vector-elem)
      (Un -Symbol -String))
    (check-equal?
      (type-step (-ivec*) 'vector-elem)
      (Un))
    (check-equal?
      (type-step (-mvec*) 'vector-elem)
      (Un))
    (check-equal?
      (type-step -Mutable-VectorTop 'vector-elem)
      Univ)

    (check-equal?
      (type-step (make-Box -Symbol) 'box-elem)
      -Symbol)
    (check-equal?
      (type-step -BoxTop 'box-elem)
      Univ)

    ;; hash
    (check-equal?
      (type-step (-Mutable-HT -Symbol -String) 'hash-key)
      -Symbol)
    (check-equal?
      (type-step (-Immutable-HT -Symbol -String) 'hash-key)
      -Symbol)
    (check-equal?
      (type-step (-Weak-HT -Symbol -String) 'hash-key)
      -Symbol)
    (check-equal?
      (type-step (-Mutable-HT -Symbol -String) 'hash-value)
      -String)
    (check-equal?
      (type-step (-Immutable-HT -Symbol -String) 'hash-value)
      -String)
    (check-equal?
      (type-step (-Weak-HT -Symbol -String) 'hash-value)
      -String)
    (check-equal?
      (type-step -Mutable-HashTableTop 'hash-key)
      Univ)
    (check-equal?
      (type-step -Weak-HashTableTop 'hash-key)
      Univ)
    (check-equal?
      (type-step -Mutable-HashTableTop 'hash-value)
      Univ)
    (check-equal?
      (type-step -Weak-HashTableTop 'hash-value)
      Univ)

    (check-equal?
      (type-step (-seq -Symbol) 'sequence-ref)
      -Symbol)
    (check-equal?
      (type-step (-seq -String -Symbol) 'sequence-ref)
      #f)
    (check-equal?
      (type-step -SequenceTop 'sequence-ref)
      Univ)
    (check-equal?
      (type-step (-seq -Symbol) 'sequence-rest)
      (-seq -Symbol))
    (check-equal?
      (type-step (-seq -String -Symbol) 'sequence-rest)
      (-seq -String -Symbol))
    (check-equal?
      (type-step -SequenceTop 'sequence-rest)
      -SequenceTop)

    (check-equal?
      (type-step (-set -Symbol) 'set-elem)
      -Symbol)
    (check-equal?
      (type-step (-set -Symbol) 'set-rest)
      (-set -Symbol))

    ;; TODO
    ;(check-equal?
    ;  (type-step (-struct #'Foo #f (list (list -Symbol '() #f) (list -String '() #f))) '(struct-elem . 0))
    ;  -Symbol)
    ;(check-equal?
    ;  (type-step (-struct #'Foo #f (list (list -Symbol '() #f) (list -String '() #f))) '(struct-elem . 1))
    ;  -String)

    ;; '(object-field . wepa)
    ;; '(object-method-rng . wepa) ;; TODO need position, for Values?


  )
)

