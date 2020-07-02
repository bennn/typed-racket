#lang racket

;; Blame 1 boundary
;; Typed method with keyword args

(module t typed/racket #:transient
  (: c% (Class (mmm (->* [] [#:x String #:y Symbol] Void))))
  (define c%
    (class object%
      (super-new)
      (define/public (mmm #:x [x "A"] #:y [y 'B])
        (void))))
  (define obj (new c%))
  (provide obj))

(require 't)

(send obj mmm #:x 'hello)

