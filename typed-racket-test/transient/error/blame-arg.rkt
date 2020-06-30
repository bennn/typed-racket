#lang typed/racket/base #:transient

;; Blame 1 boundary

(module a racket/base
  (define (abuse-callback f x)
    (f -2))
  (provide abuse-callback))

(require/typed 'a
  (abuse-callback (-> (-> String Symbol) String Symbol)))

(: my-f (-> String Symbol))
(define (my-f s)
  (string->symbol s))

(abuse-callback my-f "hello")

