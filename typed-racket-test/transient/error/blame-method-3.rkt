#lang typed/racket #:transient

;; Blame 1 boundary
;; Method with optional argument

(module a racket
  (define c%
    (class object%
      (super-new)
      (define/public (mapp f #:x [x "Hello"])
        (f 'XXXX))))
  (define c (new c%))
  (provide c))


(require/typed 'a
  (c (Instance (Class (mapp (->* [(-> String Symbol)] [#:x String] Symbol))))))

(: my-f (-> String Symbol))
(define (my-f s)
  (string->symbol s))

(ann (send c mapp my-f #:x "hi") Symbol)

