#lang typed/racket/base #:transient

;; List? should expand to a check for `list?`
;; Vector? should expand to a check for `vector?`

(define-type Big-T (List (Listof Integer) (List Symbol Integer Integer)))
(define-predicate List? Big-T)

(define-predicate Vector? (Vectorof Big-T))

(: g (-> Any (U #f Big-T)))
(define (g y)
  (if (List? y)
    y
    #f))

(void
  (Vector? 4))
