#lang racket/base

(require racket/class rackunit)

(module a racket
  (provide c%)
  (define c%
    (class object%
      (super-new)
      (define/public (m) (void)))))

(module b typed/racket #:transient
  (require/typed (submod ".." a) [c% (Class [m (-> Void)])])
  (provide o)
  (: o (Object))
  (define o (new (class c%
                   (super-new)
                   (define/public (n) (void))))))

(require 'b)

(check-not-exn
  (lambda ()
    (send o m)
    (send o n)))
