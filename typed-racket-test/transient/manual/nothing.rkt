#lang typed/racket/base #:transient

;; type Nothing does not need a codomain check
;;
;; Expand this file, look for absence of "dynamic-typecheck"

(module a racket/base
  (provide f)
  (define (f) (error 'die)))

(require/typed 'a
  (f (-> Nothing)))

(f)
