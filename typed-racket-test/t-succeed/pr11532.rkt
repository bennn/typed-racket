#lang typed/racket #:transient
(define-type T (All [X Y ...] String))
(: f (All [A] (T -> Any)))
(define f void)
