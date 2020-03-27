#lang racket/base

(module defs typed/racket/base #:transient
  (provide foo)
  (: foo Integer)
  (define foo 4)
  )

(module private-defs typed/racket/base #:transient
  (require (submod ".." defs))
  (provide foo)
  )

(require 'private-defs)
foo
