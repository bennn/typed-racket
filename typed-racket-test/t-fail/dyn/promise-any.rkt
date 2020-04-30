#lang racket

;;bg NO ERROR

(module typed typed/racket #:transient
  (: d Any)
  (define d (delay (lambda: ([x : Integer]) (+ x 1))))
  (provide d))

(require 'typed)

;; this line should raise a ctc error
((force d) 6)

