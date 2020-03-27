
#lang scheme/load

(module m typed-scheme #:transient
 (provide f)
 (: f Sexp)
 (define f 5))

(require 'm)

f
