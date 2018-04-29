#lang racket/base

;;bg: if `f : T0 -> T1`, need to guard every application of `f` because
;;    the only trustworthy part of the type is `T0`
;; TODO
;; - don't need to guard if `f` is bound to something statically typed
;;   (defined by TR, or a TR primitive like `length`)
;; - ????
;;
;; TODO when need to check (_ x ...) ?
;; - NOT when _ is a lambda (... or when is identifier for TR lambda with no untyped requires)
;; - 

(require (for-syntax racket/base) (for-template racket/base)
         syntax/parse racket/match
         typed-racket/types/type-table
         typed-racket/types/utils
         typed-racket/defender/utils)

(provide app-opt-expr)

(define (tc-result1? x)
  (match x
   [(tc-result1: _) #true]
   [_ #false]))

(define-syntax-class app-opt-expr
  #:commit
  #:literal-sets (kernel-literals)
  #:attributes (opt)
  (pattern (#%plain-app op x*:opt-expr ...)
    #:with opt
    (let ([thist (maybe-type-of this-syntax)])
      (if (tc-result1? thist)
        (dynamic-typecheck #'(#%plain-app op x*.opt ...) thist)
        #'(#%plain-app op x*.opt ...)))))
