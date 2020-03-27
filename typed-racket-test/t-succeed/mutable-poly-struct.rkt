#lang typed/racket #:transient

(define-struct: (A) X ([b : A]) #:mutable)

set-X-b!

(struct: (A) Foo ([x : Integer]) #:mutable)
(define x (Foo 10))
(set-Foo-x! x 100)
