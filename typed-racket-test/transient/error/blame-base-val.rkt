#lang typed/racket #:transient

;; Untyped provides bad base value,
;;  should get transient error that blames the one boundary

(module a racket/base
  (define fff #f)
  (provide fff))

(require/typed 'a
  (fff (-> Void)))

(fff)
