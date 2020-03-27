#lang typed/racket #:transient

(struct: x ())
(define-struct: y ())

(x) (y)

(provide (struct-out x) (struct-out y))
