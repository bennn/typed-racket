#lang typed/racket #:transient

;; Untyped class enters typed code,
;;  used to make object,
;;  but result does not match expected type
;;
;;  blame 1 boundary

(module u0 racket
  (define c%
    (class object%
      (super-new)
      (define/private (m0 x0)
        'hello)))
  (provide c%))

(require/typed 'u0
  (c% (Class (m0 (-> Symbol Symbol)))))

(: obj (Instance (Class (m0 (-> Symbol Symbol)))))
(define obj
  (new c%))

