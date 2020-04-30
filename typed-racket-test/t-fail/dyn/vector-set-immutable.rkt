#;
(exn-pred exn:fail:contract? #rx"vector-set\\!")
#lang typed/racket #:transient

(define v : (Vectorof Integer) (vector-immutable 1 2 3))
(vector-set! v 0 0)
