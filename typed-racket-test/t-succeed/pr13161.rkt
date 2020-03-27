#lang typed/racket/base #:transient

(provide (struct-out x)
         make-x)

(struct: x () #:transparent)

(: make-x : -> x)
(define (make-x)
    (x))
