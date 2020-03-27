#lang typed/racket #:transient

(define-type Animal (U cat dog))
(define-predicate animal? Animal)

(struct: cat ([lives : Natural]))
(struct: dog ([bark : Natural] [bite : Natural]))
