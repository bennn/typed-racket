#lang racket/load

(module sgn-exporter typed/racket/base #:transient
  (require/typed
   racket/math
   [sgn  (Integer -> Fixnum)])
  (provide (all-defined-out)))

(module sgn-importer typed/racket/base #:transient
  (require racket/math 'sgn-exporter))

(require 'sgn-exporter)
