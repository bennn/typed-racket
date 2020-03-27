#lang racket/load
(module stream typed/racket #:transient
  (define-type (Stream a) (Rec s (Promise (U Null (Pair a s)))))
  (provide Stream))
(module m typed/racket #:transient
  (require 'stream)
  (: x (Stream Integer))
  (define x (delay '())))
(require 'm)
