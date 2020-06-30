#lang typed/racket #:transient

;; Blame 1 boundary

(module a racket
  (define c%
    (class object%
      (super-new)
      (define/public (mapp f x)
        (f 'XXXX))))
  (define c (new c%))
  (provide c))


(require/typed 'a
  (c (Instance (Class (mapp (-> (-> String Symbol) String Symbol))))))

(: my-f (-> String Symbol))
(define (my-f s)
  (string->symbol s))

(ann (send c mapp my-f "hello") Symbol)

