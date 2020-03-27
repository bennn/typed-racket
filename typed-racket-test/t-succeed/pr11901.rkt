#lang typed/racket/base #:transient
(define-type (adder lhs rhs) (lhs rhs -> Number))
(define-struct: (lhs rhs) adder-box ((a : adder)))
