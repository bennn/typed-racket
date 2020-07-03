#lang typed/racket/base #:transient

;; Blame 1 boundary

(module a racket/base
  (define v (vector #f))
  (define (callback)
    (define f (vector-ref v 0))
    (f 'XXXX))
  (provide v callback))

(require/typed 'a
  (callback (-> Void))
  (v (Vectorof (-> String Symbol))))

(: my-f (-> String Symbol))
(define (my-f s)
  (string->symbol s))

(vector-set! v 0 my-f)
(callback)

