#lang racket

;; Blame 1 boundary
;; Typed method

(module t typed/racket #:transient
  (: c% (Class (mmm (-> String Symbol Void))))
  (define c%
    (class object%
      (super-new)
      (define/public (mmm str sym)
        (void))))
  (define obj (new c%))
  (provide obj))

(require 't)

(send obj mmm 'hello 'hello)


