#lang typed/racket/base #:transient

;; Blame 1 boundary,
;;  call-with-values returns bad result

(module u racket/base
  (provide fff)
  (define (fff) (void)))

(require/typed 'u
  (fff (-> Symbol)))

(call-with-values values fff)

