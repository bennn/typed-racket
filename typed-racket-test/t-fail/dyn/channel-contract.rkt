#lang racket/load

;; Test typed-untyped interaction with channels
;;bg NO ERROR

(module typed typed/racket #:transient
  (: ch (Channelof (Boxof Integer)))
  (define ch (make-channel))
  (: putter (-> Thread))
  (define (putter)
    (thread (λ () (channel-put ch (box 3)))))
  (provide putter ch))

(require 'typed)
(putter)
(set-box! (channel-get ch) "not an integer")

