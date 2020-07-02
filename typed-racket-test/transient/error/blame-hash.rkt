#lang typed/racket/base #:transient

;; Blame 1 boundary

(module a racket/base
  (define h (make-hash (list (cons 'a #f))))
  (define (callback)
    (define f (hash-ref h 'a #f))
    (f 'XXXX))
  (provide h callback))

(require/typed 'a
  (callback (-> Void))
  (h (HashTable Symbol (-> String Symbol))))

(: my-f (-> String Symbol))
(define (my-f s)
  (string->symbol s))

(hash-set! h 'a my-f)
(callback)


