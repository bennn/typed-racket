#lang at-exp typed/racket #:transient

(define contents
 (lambda args args))

(define doc @contents{x y})

@contents{x y}
