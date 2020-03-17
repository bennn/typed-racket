#lang racket/base

;; Expect type->contract cache hits for both these

(module a typed/racket/base #:guarded
  (require/typed racket/base
    ((void v0) (-> Integer Void)))
  (require/typed racket/base
    ((void v1) (-> Integer Void))))


(module b typed/racket/base #:transient
  (require/typed racket/base
    ((void v0) (-> Integer Void)))
  (require/typed racket/base
    ((void v1) (-> Integer Void))))

(require 'a 'b)

