#lang racket/base

;; Test providing a value to an untyped context

;; 2019-07-22 is this file correct? what's it testing?
;; - (f 0) should be a contract error
;; - so should evaluating the body of 'c

(module a typed/racket/base
        (provide f)
        (define (f (x : (Boxof (Boxof Integer))))
          (unbox (unbox x))))

(module b racket/base
        (provide bbx)
        (define bbx (box 0)))

(module c typed/racket/base #:locally-defensive
        (require (submod ".." a))
        (require/typed (submod ".." b) (bbx (Boxof (Boxof Integer))))
        (provide do-c)
        (define (do-c)
          (f bbx)))

(require 'a rackunit)
(check-exn #rx"f: contract violation"
           (lambda () (f 0)))
(require 'c)
(check-exn #rx"f: contract violation"
           do-c)
