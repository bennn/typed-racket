#lang typed/scheme #:transient



(define-type (T+ elem)
  (U (A elem)))

(define-type (T elem)
  (U T+ 2))

(define-struct: (x) A ())
