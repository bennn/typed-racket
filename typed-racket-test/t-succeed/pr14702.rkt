#lang typed/racket #:transient
(define (f [x : Integer]) : Integer (f x))
