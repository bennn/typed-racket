#lang typed/racket #:transient
(require racket/syntax)

(: f : -> (Syntaxof Any))
(define (f)
  (with-syntax* ([(x ...) (list 1 2 3)])
    #`(#,(syntax +) x ...)))
  
