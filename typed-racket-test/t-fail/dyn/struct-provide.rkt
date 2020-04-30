#lang scheme/load

;;bg NO ERROR

(module m typed-scheme #:transient
  (require (for-syntax scheme/base))
  (define-syntax (q stx) #'#f)
  (provide (all-defined-out)))

(module n scheme
  (require 'm)
  q)
