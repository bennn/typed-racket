#lang typed/racket/base #:no-optimize #:transient
(require typed/racket/class)

(define c%
  (class object%
    (init-field val)
    (super-new)))

(instantiate c% (3))
