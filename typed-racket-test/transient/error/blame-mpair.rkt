#lang typed/racket/base #:transient

;; Blame 1 boundary

(module a racket/base
  (define m (mcons #f #f))
  (define (callback)
    (define f (mcdr m))
    (f 'XXXX))
  (provide m callback))

(require/typed 'a
  (callback (-> Void))
  (m (MPairof Void (-> String Symbol))))

(: my-f (-> String Symbol))
(define (my-f s)
  (string->symbol s))

(set-mcdr! m my-f)
(callback)


