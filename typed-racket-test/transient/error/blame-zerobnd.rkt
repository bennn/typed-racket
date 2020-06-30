#lang typed/racket/base #:transient

;; Example where transient blames 0 boundaries,
;;  a typed function gets bad input,
;;  and this function is NOT in the blame map
;;  because it "escaped" to untyped without going through a cast
;;
;; Easy to do because cast-sites are static and escape paths can
;;  appear at runtime. `map` is another way to escape.

(module a racket/base
  (define xs '(A B C))
  (provide xs))

(require/typed 'a
  (xs (Listof String)))

(: my-f (-> String Symbol))
(define (my-f s)
  (string->symbol s))

(filter my-f xs)

