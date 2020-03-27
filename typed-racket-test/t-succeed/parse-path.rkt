#lang typed/scheme #:transient

(: f ((Pair Any Any) -> Boolean : Number @ car))
(define f (lambda: ([x : (Pair Any Any)]) (number? (car x))))
