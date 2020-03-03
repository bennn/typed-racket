#lang typed/racket/base #:transient

(define-type Big-T (List (Listof Integer) (List Symbol Integer Integer)))
(define-predicate F Big-T)


(: g (-> Any (U #f Big-T)))
(define (g y)
  (if (F y)
    y
    #f))
