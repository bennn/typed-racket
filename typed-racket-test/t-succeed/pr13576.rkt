#lang typed/racket/base #:transient

(define: (A ...) (lister args : A ... A) : (List A ... A)
   args)
