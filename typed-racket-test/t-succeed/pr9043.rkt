#lang scheme/load

(module foo scheme

  (define-struct foo (x y))
  (provide (struct-out foo)))

(module client typed-scheme #:transient
  (require-typed-struct foo ([x : Number] [y : Symbol]) 'foo))

