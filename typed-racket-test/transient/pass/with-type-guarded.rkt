#lang racket/base

(require (only-in typed/racket/base with-type String))

(let ([x "hello"])
  (with-type #:result String
    #:freevars ([x String])
    (string-append x " world")))
