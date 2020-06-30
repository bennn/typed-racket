#lang typed/racket/base #:transient

;; Blame 1 boundary

(module a racket/base
  (define b (box #f))
  (define (callback)
    (define f (unbox b))
    (f 'XXXX))
  (provide b callback))

(require/typed 'a
  (callback (-> Void))
  (b (Boxof (-> String Symbol))))

(: my-f (-> String Symbol))
(define (my-f s)
  (string->symbol s))

(set-box! b my-f)
(callback)

