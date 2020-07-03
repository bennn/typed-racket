#lang typed/racket/base #:transient

;; Blame 1 boundary

(module a racket/base
  (define v0 (vector #f))
  (define v1 (vector #f))
  (define (callback)
    (define f (vector-ref v0 0))
    (f 'XXXX))
  (provide v0 v1 callback))

(require/typed 'a
  (callback (-> Void))
  (v0 (Vectorof (-> String Symbol)))
  (v1 (Vectorof (-> String Symbol))))

(: my-f (-> String Symbol))
(define (my-f s)
  (string->symbol s))

(vector-set! v1 0 my-f)
(vector-copy! v0 0 v1 0)
(callback)

