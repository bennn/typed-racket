#lang typed/racket/base #:transient

;; LD submodules in LD code

(module t typed/racket/base #:transient
  (: f (-> Real (-> Real Real) Real))
  (define (f r g)
    (g (g r)))
  (provide f))
(require 't)

(: h (-> Real Real))
(define (h n)
  (* n n))

(f 3 h)
