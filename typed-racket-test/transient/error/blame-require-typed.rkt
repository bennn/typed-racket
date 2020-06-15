#lang typed/racket/base #:transient

;; Function crosses two require/typed boundaries,
;;  only one should be blamed.

(module u racket/base
  (define (g x) x)
  (define (h y) (y 21))
  (define cnst42 (lambda (n) 42))
  (provide g h cnst42))

(require/typed 'u
  (g (-> (-> Real Real) (-> Real Real)))
  (h (-> (-> Real String) String))
  (cnst42 (-> Real Nothing)))

(void (g cnst42))
(h cnst42)


