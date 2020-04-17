#lang typed/racket/base #:transient

(: f0 (case-> (-> Symbol Symbol)))
(define f0
  (case-lambda
    [(x) x]))

(f0 'x)
((case-lambda [(x) x]) 'x)
((case-lambda [(x) x] [(y z) z]) 'x)
