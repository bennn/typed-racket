#lang typed/racket/base #:transient

(define #:forall (A ...) (f [v : (List A ... A)])
  (apply values v))

(f '(1 2 3))
