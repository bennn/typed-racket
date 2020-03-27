#lang typed/racket #:transient

(provide foo)

(define foo
 (case-lambda:
   (((x : Number)) x)
   (((x : Number) (y : Number) z : Number *) y)))
