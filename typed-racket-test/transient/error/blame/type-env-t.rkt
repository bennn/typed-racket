#lang typed/racket/base

(define-type R Real)
(define-type S String)
(define-type RR (-> R R))

(provide R S RR)