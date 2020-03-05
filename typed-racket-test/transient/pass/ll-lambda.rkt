#lang racket/base

;; Test tc-app lambda special case for ((lambda ...) ...)

(module a typed/racket #:guarded
  (: a Integer)
  (: b (Listof Symbol))
  (define-values [a b]
     ((lambda ((x : Integer)) (values x (list 'A 'A))) 42)))

(module r racket/base
  (provide f)
  (define (f x) (values x '(A A))))

(module b typed/racket #:transient
  (: x0 Integer)
  (: x1 (Listof Symbol))
  (define-values [x0 x1]
     ((lambda ((x : Integer)) (values x (list 'A 'A))) 42))

  (require/typed
    (submod ".." r)
    (f (-> Integer (Values Integer (Listof Symbol)))))

  (: x2 Integer)
  (: x3 (Listof Symbol))
  (define-values [x2 x3]
     ((lambda ((x : Integer)) (f x)) 42))

  (: x4 Integer)
  (: x5 (Listof Symbol))
  (define-values [x4 x5]
     (f 42))
  )

(require 'a 'b)
