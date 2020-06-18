#lang racket/base

;; Untyped object enters typed module,
;;  method call returns 2 results and 2nd is bad,
;;  blame 1 boundary
;;
;; (blame-filtering should not fail here)

(module u0 racket
  (define c%
    (class object%
      (super-new)
      (field
        [f0 'aaa]
        [f1 "bbb"])
      (define/public (m0 x0)
        'hello)
      (define/public (m1 x1)
        (values 'hello "hello"))))
  (define o (new c%))
  (provide o))

(module t0 typed/racket #:transient
  (define-type C0%
    (Class
      (field (f0 Symbol) (f1 String))
      [m0 (-> Symbol Symbol)]
      [m1 (-> String (Values Symbol Symbol))]))
  (require/typed (submod ".." u0)
    (o (Instance C0%)))
  (: s0 Symbol)
  (: s1 Symbol)
  (define-values [s0 s1] (send o m1 "a")))

(require 't0)

