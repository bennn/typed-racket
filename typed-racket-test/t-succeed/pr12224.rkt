#lang racket/load

(module t1 typed/racket/base #:transient
  (provide (all-defined-out))
  (define-struct: f ([n : ((Promise Number) -> Number)])))

(module t2 typed/racket/base #:transient
  (require racket/promise 't1)
  (: g (f (Promise Number) -> Number))
  (define (g fx k)
      ((f-n fx) k)))
