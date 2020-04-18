#lang typed/racket/base #:transient

;; Make sure defender runs in module*
;;  ... ideally WITHOUT the #:transient flag, but alas

(module u racket/base
  (define b
    (box "hello"))
  (provide b))

(require/typed 'u (b (Boxof Integer)))

(module* test #f
  #:transient
  (require typed/rackunit)

  (check-exn exn:fail:contract?
    (lambda () (unbox b))))
