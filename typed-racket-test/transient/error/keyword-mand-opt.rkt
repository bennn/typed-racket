#lang typed/racket/base #:transient

;; ERROR when function asks for mandatory kw
;;  but type makes it optional

(module u racket/base
  (define (f0 x #:y y)
    (void))
  (provide f0))

(require/typed 'u
  (f0 (->* [Symbol] [#:y Symbol] Void)))

(void f0)
