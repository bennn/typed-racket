#lang typed/scheme #:transient

(: foo : (Integer -> Integer -> Integer))
(define ((foo x) y) 1)
