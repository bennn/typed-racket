#lang typed/racket #:transient

;; Make sure the Custodian type is bound

(: cust Custodian)
(define cust (current-custodian))

