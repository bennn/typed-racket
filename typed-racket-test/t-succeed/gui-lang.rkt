#lang typed/racket/gui #:transient

;; Test typed/racket/gui used as a language

(define (f #{x : Integer}) (add1 x))
(f 3)

(make-object bitmap% 300 300)
