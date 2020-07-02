#lang racket

;; Send typed class across boundary,
;;  NEW happens in untyped code,
;;  able to trace because method blames both object & class
;;
;; Blame 1

(module t typed/racket #:transient
  (: c% (Class (mmm (->* [] [#:x String #:y String] Void))))
  (define c%
    (class object%
      (super-new)
      (define/public (mmm #:x [x "A"] #:y [y 'B])
        (void))))
  (provide c%))

(require 't)

(define obj (new c%))

(send obj mmm #:x #f)

