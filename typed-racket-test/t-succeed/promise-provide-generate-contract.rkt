#lang typed/racket #:transient

(struct (A) s ([f : Any]))
(define p (delay (s "a")))
(provide p)
