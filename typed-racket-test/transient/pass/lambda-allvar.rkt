#lang typed/racket/base #:transient

;; From the `math` library, something like this appears after macro expansion
;;  and transient was refusing to defend because had 'Error' type.
;;
;; Missing a type annotation?
;; If not, maybe TR is over-specializing.

(: fff (All (A) (-> A Void)))
(define (fff ba)
  (define ggg
    (lambda ((v : A)) (void)))
  (void))
