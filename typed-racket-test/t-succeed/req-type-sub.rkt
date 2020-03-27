#lang racket

(define (f x) (add1 x))
(provide f)
(module* m typed/racket #:transient
  (require/typed (submod "..") [f (Integer -> Integer)])
  (f 7))

(module* n typed/racket #:transient
  (require/typed (submod "..") [f (Integer -> String)])
  (f 7))
