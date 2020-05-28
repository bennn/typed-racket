#lang racket/base

;; 2020-05-27 : static type error
;; ... shouldn't this work though?

(module t typed/racket/base #:transient
  (struct foo ((a : String)) #:property prop:procedure (lambda ((f : foo) (x : Symbol)) (symbol->string x)))
  (provide (struct-out foo)))
(require 't)

(define myf (foo "hello"))
(myf 42)
