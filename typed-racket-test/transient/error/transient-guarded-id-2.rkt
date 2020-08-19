#lang typed/racket/base

;; cannot send syntax transient -> guarded,
;;  (should only be error when running the guarded module, gotta test separately)

(module transient typed/racket/base #:transient
  (provide xxx)
  (: xxx (Syntaxof Any))
  (define xxx
    #`#,(vector 0)))

(require 'transient)
xxx

