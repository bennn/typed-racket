#lang typed/racket/base #:transient

(struct: (v) a ((x : v)) #:mutable)

(: z (Struct (Rec b (a (U #f b)))))
(define z (a #f))
