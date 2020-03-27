#lang typed/racket #:transient


(: v (U (Vector String) (Vector Symbol)))
;(define v (vector "hello"))
(define v (vector 'hello))
