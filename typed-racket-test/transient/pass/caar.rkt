#lang typed/racket/base #:transient

;; Test that nested elimination forms get checked

(define (f)
  (car (car '((1)))))
