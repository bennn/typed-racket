#lang typed/scheme #:transient
(: lon? (Any -> Boolean : (Listof Number)))
(define (lon? x)
  (and (list? x) (andmap number? x)))
